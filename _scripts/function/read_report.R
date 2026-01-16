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
    compare_tables <- tbls[grepl("^.*(ObsPred|PredictedObserved).*$", tbls, ignore.case = TRUE)]
    if (length(compare_tables) == 0) {
        warning("No observation vs prediction tables found in the database: ", file)
        return(NULL)
    }
    reports <- list()
    i <- 1
    for (i in seq_along(compare_tables)) {
        tbl_i <- DBI::dbReadTable(con, compare_tables[i]) |> tibble::tibble()
        cols <- c("SimulationID")
        if (tibble::has_name(tbl_i, "Clock.Today")) {
            cols <- c(cols, "Clock.Today")
        }
            
        tbl_i <- tbl_i  |> 
            dplyr::select(c(cols, starts_with("Predicted"), starts_with("Observed"))) |> 
            dplyr::select(-starts_with("Pred.Obs")) 
        cols_del <- c()
        if (tibble::has_name(tbl_i, "Observed.Clock.Today")) {
            cols_del <- c(cols_del, "Observed.Clock.Today")
        }
        if (tibble::has_name(tbl_i, "Predicted.Clock.Today")) {
            cols_del <- c(cols_del, "Predicted.Clock.Today")
        }
        if (length(cols_del) > 0) {
            tbl_i <- tbl_i |> 
                dplyr::select(-dplyr::all_of(cols_del))
        }

        suppressWarnings({
        tbl_i <- tbl_i  |> 
            dplyr::mutate(dplyr::across(c( starts_with("Predicted"), starts_with("Observed")), ~as.numeric(.x))) |> 
            tidyr::pivot_longer(cols = c( starts_with("Predicted"), starts_with("Observed")))
        })
        reports[[i]] <- tbl_i
    }
    reports <- dplyr::bind_rows(reports)

    reports <- reports |> 
        dplyr::filter(grepl(crop, name)) |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::mutate(type = gsub("^(Observed|Predicted)\\..+$", "\\1", name),
            trait = gsub("^(Observed|Predicted)\\.(.+)$", "\\2", name)) |> 
        
        dplyr::select(-name) |> 
        dplyr::group_by(SimulationID, type, trait, Clock.Today) |> 
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
    sims <- sims |> 
        dplyr::mutate(Crop = crop,
            APSIMX = basename(apsimx))
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

.get_experiments <- function(apsimx) {
    stopifnot(is.character(apsimx) && length(apsimx) == 1)
    model <- rapsimng::read_apsimx(apsimx)
    experiments <- rapsimng::search_node(model, "$type" = "Models.Factorial.Experiment, Models", all = TRUE)
    i <- 1
    exp_info <- list()
    for (i in seq(along = experiments)) {
        memo_node <- rapsimng::search_node(experiments[[i]]$node, "$type" = "Models.Memo, Models")
        exp_info_i <- data.frame(Experiment = experiments[[i]]$node$Name, Memo = NA_character_)
        if (length(memo_node) > 0) {
            if (length(memo_node$path) == 2) {
                exp_info_i$Memo <- memo_node$node$Text
            }
        }
        exp_info[[i]] <- exp_info_i
    }
    exp_info <- do.call(rbind, exp_info)
    exp_info
}

get_experiments <- function(apsimxs) {
    stopifnot(is.character(apsimxs))
    exp_infos <- lapply(apsimxs, function(x) {
        .get_experiments(x)
    })
    exp_infos <- do.call(rbind, exp_infos) |> 
        tibble::tibble()
    exp_infos
}
