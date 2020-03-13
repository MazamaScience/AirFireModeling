raster_load <- function(
  model = 'PNW-4km',
  run = NULL,
  xlim = NULL,
  ylim = NULL,
  dirLocal = NULL,
  dirURL = 'https://haze.airfire.org/bluesky-daily/output/standard',
  type = 'forecast',
  clean = FALSE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------


  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(dirURL)

  if ( is.null(run) ) { # Use todays date if run is null
    now <- lubridate::now(tzone = 'UTC')
    run <- paste0(strftime(now, '%Y%m%d', tz = 'UTC'), '00')
  } else {
    # Make sure we end up with YYYYmmddHH
    run <- as.character(run)
    length <- stringr::str_length(run)[1]
    if ( length == 8 ) { # if the length is 8 append 00 (model runs work YYYYmmddHH)
      run <- paste0(run, '00')
    }
  }
  # Verify YYYYmmddHH
  if ( !stringr::str_detect(run, '[0-9]') ) {
    stop("'run' parameter must have 10 digits")
  }
  # Default to cleanup
  if ( !is.logical(clean) ) {
    warning('"clean" parameter must be logical boolean. Defaulting to FALSE')
    clean <- FALSE
  }
  # Default to verbose
  if ( !is.logical(verbose) ) {
    warning('"verbose" parameter must be logical boolean. Defaulting to TRUE')
    verbose <- TRUE
  }

  # ----- Load -----
  # Create a Parallel Socket Cluster
  cl <- parallel::makeCluster(future::availableCores() - 1)
  future::plan(strategy = future::cluster, workers = cl)
  data_dir <- getModelDataDir()

  model_list <- list()

  # Create a list of models to load parallel
  for ( i in model ) {
    model_list[[i]] <- future::future({
      setModelDataDir(data_dir)
      .bluesky_load(model, run, xlim, ylim, dirLocal, dirURL, type, clean, verbose)
    })
  }
  cat("Loading ")
  while(!future::resolved(model_list[[1]])) {
    cat("=")
    Sys.sleep(2)
  }
  cat("\n")

  models <- future::values(model_list)
  parallel::stopCluster(cl)
  return(models)

}
