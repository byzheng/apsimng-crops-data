# Run apsimx files 
get_apsimx <- function(crops) {
    stopifnot(is.character(crops) && length(crops) >= 1)
    base_folder <- Sys.getenv("APSIMX_DIR")
    if (nchar(base_folder) == 0) {
        stop("Please set the APSIMX_DIR environment variable to the APSIMX directory.")
    }
    
    # List APSIMX for each crop
    all_files <- NULL
    i <- 1
    for (i in seq(along = crops)) {
        crop <- crops[i]
        crop_dir <- file.path(base_folder, sprintf("Tests/Validation/%s/", crop))
        files <- list.files(crop_dir, "\\.apsimx$", full.names = TRUE, recursive = TRUE)

        if (length(files) == 0) {
            warning(sprintf("No .apsimx files found for crop %s in %s", crop, crop_dir))
            next
        }
        all_files[[i]] <- tibble::tibble(
            crop = crop,
            file = files
        )
    }
    all_files <- dplyr::bind_rows(all_files)
    return(all_files)
}
