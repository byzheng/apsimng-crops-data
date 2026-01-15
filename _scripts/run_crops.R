rm(list = ls())

rerun <- TRUE # whether to rerun apsimx simulations
target_crops <- c("Barley", "Wheat", "Canola") # list of crops to process
APSIMX_DIR <- Sys.getenv("APSIMX_DIR")

crop_output_dir <- "_outputs" # Directory to store cached data

# source all functions
a <- list.files("_scripts/function", full.names = TRUE) |>
    lapply(source)

# create cache directory
if (!dir.exists(crop_output_dir)) {
    dir.create(crop_output_dir, recursive = TRUE)
}
# list all crops and only keep target crops
models <- list.files(file.path(APSIMX_DIR, "Models/Resources/"), "*.json", full.names = TRUE)
crops <- tibble::tibble(Model = models) |> 
    dplyr::mutate(Crop = tools::file_path_sans_ext(basename(Model)))
crops <- crops |> 
    dplyr::filter(Crop %in% target_crops)
i <- 1
for (i in seq(along = crops[[1]])) {
    crop <- crops$Crop[i]
    # List all apsimx files
    files <- get_apsimx(crop)
    # files <- files[7,]
    # Run apsimx files
    run_apsimx(files$file, apsimx_base = APSIMX_DIR, rerun = rerun)

    # read all reports 
    all_reports <- read_reports(files$file, crop) |> 
        dplyr::mutate(Genotype = tolower(Genotype))
    obs_file <- file.path(crop_output_dir, paste0(crop, ".Rds"))
    saveRDS(all_reports, obs_file)
}
