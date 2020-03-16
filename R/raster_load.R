#' @title Load a Raster Model
#'
#' @param model Model identifier(s).
#' @param run Date code as "YYYYMMDDHH" (integer or character).
#' @param xlim Optional longitude range.
#' @param ylim Optional latitude range.
#' @param local Absolute path of the local NetCDF (.nc) file
#' @param dirURL Model output web directory. Default is BlueSky standard output.
#' @param type Model type directory, i.e. 'forecast', 'combined', etc.
#' @param clean Logical specifying whether to remove the non-common format NetCDF.
#' @param verbose If \code{FALSE}, suppress status messages (if any).
#'
#' @return A raster object, or a list of raster objects
#' @export
#'
#' @examples
#' \dontrun{
#' # Set Model download directory (Required)
#' setModelDataDir('~/Data/Bluesky')
#' # Load from server
#' model <- raster_load('PNW-1.33km', run = 20200303, xlim = c(-125, -115))
#' # Load from local NetCDF
#' model <- raster_load('~/Data/Bluesky/PNW-4km_2020030100.nc')
#' }
raster_load <- function(
  model = 'PNW-4km',
  run = NULL,
  xlim = NULL,
  ylim = NULL,
  local = NULL,
  dirURL = 'https://haze.airfire.org/bluesky-daily/output/standard',
  type = 'forecast',
  clean = FALSE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------
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
  if ( (length(model) >= 1) && is.null(local) && (length(run) <= 1) ) {
    for ( i in model ) {
      model_list[[i]] <- future::future({
        setModelDataDir(data_dir)
        .bluesky_load(i, run, xlim, ylim, local, dirURL, type, clean, verbose)
      })
    }
    cat("Loading Model")
    while(!future::resolved(model_list[[1]])) {
      cat(".")
      Sys.sleep(2)
    }
    cat("\n")
  }

  if ( length(local) >= 1 ) {
    for ( i in local ) {
      model_list[[i]] <- future::future({
        setModelDataDir(data_dir)
        .bluesky_load(model, run, xlim, ylim, i, dirURL, type, clean, verbose)
      })
    }
    cat("Loading Local Model ")
    while(!future::resolved(model_list[[1]])) {
      cat(".")
      Sys.sleep(2)
    }
    cat("\n")
  }

  if ( length(run) >= 1 ) {
    for ( i in run ) {
      model_list[[i]] <- future::future({
        setModelDataDir(data_dir)
        .bluesky_load(model, i, xlim, ylim, local, dirURL, type, clean, verbose)
      })
    }
  }

  models <- future::values(model_list)
  parallel::stopCluster(cl)
  return(models)

}
