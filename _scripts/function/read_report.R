read_report <- function(apsimx, crop) {
    stopifnot(is.character(apsimx) && length(apsimx) == 1)
    stopifnot(is.character(crop) && length(crop) == 1)    
    file <- paste0(tools::file_path_sans_ext(apsimx), ".db")
    if (!file.exists(file)) {
        warning("DB file is not found: ", file) 
        return(NULL)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    tbls <- DBI::dbListTables(con)
    if (!("SowingReport" %in% tbls)) {
        warning("SowingReport table is not found in the database: ", file)
        return(NULL)
    }
    simulations <- DBI::dbReadTable(con, "_Simulations") |> tibble::tibble()
    sowing_report <- DBI::dbReadTable(con, "SowingReport") |> tibble::tibble()
    harvest_report <- NULL
    if ("HarvestReport" %in% tbls) {
        harvest_report <- DBI::dbReadTable(con, "HarvestReport") |> tibble::tibble()
    }
    factors <- DBI::dbReadTable(con, "_Factors") |> tibble::tibble()
    reports <- tbls[grepl("(Harvest.*ObsPred|PredictedObserved)$", tbls, ignore.case = TRUE)] |> 
        purrr::map_df(function(x) {
            DBI::dbReadTable(con, x) |> 
                dplyr::select(SimulationID, starts_with("Predicted"), starts_with("Observed")) |> 
                dplyr::select(-starts_with("Pred.Obs")) |> 
                dplyr::mutate(dplyr::across(c( starts_with("Predicted"), starts_with("Observed")), ~as.numeric(.x)))
        }) |> 
        tibble::tibble() |> 
        tidyr::pivot_longer(cols = c( starts_with("Predicted"), starts_with("Observed"))) |> 
        dplyr::filter(grepl(crop, name)) |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::mutate(type = gsub("^(Observed|Predicted)\\..+$", "\\1", name),
            trait = gsub("^(Observed|Predicted)\\.(.+)$", "\\2", name)) |> 
        
        dplyr::select(-name) |> 
        dplyr::group_by(SimulationID, type, trait) |> 
        dplyr::summarise(value = mean(value), .groups = "drop") |> 
        tidyr::pivot_wider(names_from = "type",
                    values_from = "value") |> 
        dplyr::filter(!is.na(Observed))
    DBI::dbDisconnect(con)
    
    # sowing_report |>
    #     select(SimulationID, Genotype = contains('SowingData.Cultivar')) |> 
    #     filter(is.na(Genotype))
    
    sims <- sowing_report |>
        dplyr::mutate(Clock.Today = as.Date(Clock.Today)) |> 
        dplyr::select(SimulationID, Genotype = contains('SowingData.Cultivar'),
                Sowing = Clock.Today,
                Latitude = IWeather.Latitude,
                Longitude = IWeather.Longitude) |>
        dplyr::right_join(simulations,
                    by = c("SimulationID" = "ID")) |>
        dplyr::mutate(Genotype = tolower(Genotype)) |>
        tibble::tibble() |> 
        dplyr::right_join(reports, by = "SimulationID") |> 
        dplyr::left_join(factors |> 
                        #select(-FolderName, -CheckpointID) |>
                        dplyr::select(SimulationID, ExperimentName) |>
                        dplyr::distinct(), by = "SimulationID") |>
        dplyr::rename(SimulationName = Name) 
    
    # Merge emregenc date
    if (!is.null(harvest_report)) {
        emergence_report <- harvest_report |>
            dplyr::select(SimulationID, contains("EmergenceDAS")) |> 
            dplyr::rename(EmergenceDAS = contains("EmergenceDAS"))
        sims <- sims |> 
            dplyr::left_join(emergence_report, by = "SimulationID")
    }
    # simple_sims <- simplifySimulation(factors)
    # sims <- simple_sims |> 
    #     distinct() |> 
    #     left_join(sims |> distinct(SimulationID, SimulationName),
    #               by = c("TargetSimulationID" = "SimulationID")) |>  
    #     select(SimulationID, TargetSimulationName = SimulationName) |> 
    #     right_join(sims, by = "SimulationID")
    sims
}

read_reports <- function(apsimxs, crop) {
    stopifnot(is.character(apsimxs))
    stopifnot(is.character(crop) && length(crop) == 1)    
    sims_list <- lapply(apsimxs, function(x) {
        read_report(x, crop)
    })
    sims <- do.call(rbind, sims_list)
    sims
}