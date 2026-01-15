# Get the time of the latest git commit for a specific folder
.last_apsimx_commit <- local({
    cache <- new.env(parent = emptyenv())
    
    function(folder) {
        stopifnot(is.character(folder) && length(folder) == 1)
        stopifnot(dir.exists(folder))
        
        # Use normalized path as cache key
        cache_key <- normalizePath(folder, winslash = "/", mustWork = TRUE)
        
        if (!exists(cache_key, envir = cache)) {
            tryCatch({
                git_log <- system(
                    sprintf("git -C \"%s\" log -1 --format=%%ct", folder),
                    intern = TRUE,
                    ignore.stderr = TRUE
                )
                if (length(git_log) == 0 || nzchar(git_log) == 0) {
                    commit_time <- Sys.time()
                } else {
                    commit_time <- as.POSIXct(as.numeric(git_log), origin = "1970-01-01")
                }
                assign(cache_key, commit_time, envir = cache)
            }, error = function(e) {
                assign(cache_key, Sys.time(), envir = cache)
            })
        }
        
        get(cache_key, envir = cache)
    }
})

# Check if APSIMX needs to be re-run and run it
.is_rerun <- function(file, apsimx_base) {
    stopifnot(is.character(file) && length(file) == 1)
    stopifnot(file.exists(file))
    
    # return TRUE if no db file exists
    db_file <- gsub("\\.apsimx$", ".db", file)
    if (!file.exists(db_file)) {
        message("DB file not found, need to rerun: ", file)
        return(TRUE)
    }
    
    # Return TRUE if apsimx file is newer than db file
    file_info <- file.info(file)
    file_mtime <- file_info$mtime
    db_info <- file.info(db_file)
    db_mtime <- db_info$mtime
    message("APSIMX file time: ", file_mtime)
    message("DB file time: ", db_mtime)
    if (db_mtime < file_mtime) {
        message("APSIMX file is newer than DB file, need to rerun: ", file)

        return(TRUE)
    }
    
    # Return TRUE if APSIMX repo has newer commit than db file
    stopifnot(is.character(apsimx_base) && length(apsimx_base) == 1)
    stopifnot(dir.exists(apsimx_base))
    apsimx_latest <- .last_apsimx_commit(apsimx_base)
    message("APSIMX latest commit time: ", apsimx_latest)
    if (db_mtime < apsimx_latest) {
        message("APSIMX repo has newer commit than DB file, need to rerun: ", file)        
        return(TRUE)
    }
    # Otherwise return FALSE
    message("No need to rerun APSIMX for: ", file)
    return(FALSE)
}

run_apsimx <- function(files, apsimx_base, rerun = TRUE) {

    stopifnot(is.character(files) && length(files) >= 1)
    stopifnot(all(file.exists(files)))
    stopifnot(is.character(apsimx_base) && length(apsimx_base) == 1)
    stopifnot(dir.exists(apsimx_base))
    stopifnot(is.logical(rerun) && length(rerun) == 1)
    
    # Check the Models executable 
    Models <- if (.Platform["OS.type"] == "windows") {
        "Models.exe"
    } else if (.Platform["OS.type"] == "unix"){
        "Models"
    } else {
        stop("Unsupported OS type")
    }
    
    if (nzchar(Sys.which(Models)) == 0) {    
        return(invisible())
    }
    
    if (!rerun) {
        return(invisible())
    }
    # Run APSIMX for file
    i <- 1
    for (i in seq(along = files)) {
        if (!rerun) {
            next
        }
        is_rerun <- .is_rerun(files[i], apsimx_base = apsimx_base)
        if (!is_rerun) {
            message(sprintf("Skipping simulation for %s (up-to-date).", basename(files[i])))
            next
        }
        # Remove db files to force re-run
        db_files <- gsub("\\.apsimx$", ".db", files[i])
        aa <- db_files |> lapply(function(x) {
            if (file.exists(x)) {
                file.remove(x)
            }
        })
        Sys.sleep(1)  # Wait for file removal to complete
        # Run simulations
        message(sprintf("Re-running simulation(s) for %s.", files[i]))
        cmd <- paste0(Models, " \"",  files[i], "\"")
        system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        Sys.sleep(1)  # Wait for simulations to complete
    }
}
