#' @title Load/Download a Raster Model
#'
#' @param model Model identifier(s).
#' @param modelRun Date code as "YYYYMMDDHH" (integer or character).
#' @param xlim Optional longitude range.
#' @param ylim Optional latitude range.
#' @param localPath Absolute path of the local NetCDF (.nc) file
#' @param baseUrl Model output web directory. Default is BlueSky standard output.
#' @param modelType Model type directory, i.e. 'forecast', 'combined', etc.
#' @param clean Logical specifying whether to remove the non-common format NetCDF.
#' @param verbose If \code{FALSE}, suppress status messages (if any).
#'
#' @description Load or download models and automatically convert NetCDF (.nc) format to
#' a spatio-temporal \code{Raster*} object.
#'
#' @details \code{model}, \code{modelRun}, and \code{localPath} are all vectorised
#' parameters. As such, loading a model is executed using multiple CPU threads.
#'
#' @return A raster object, or a list of raster objects
#' @export
#'
#' @examples
#' \dontrun{
#' # Set Model download directory (Required)
#' setModelDataDir('~/Data/BlueSky')
#' # Load from server
#' model <- raster_load('PNW-1.33km', modelRun = 20200303, xlim = c(-125, -115))
#' # Load from local NetCDF
#' model <- raster_load('~/Data/BlueSky/PNW-4km_2020030100.nc')
#' }
raster_load <- function(
  model = 'PNW-4km',
  modelRun = NULL,
  xlim = NULL,
  ylim = NULL,
  localPath = NULL,
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  modelType = 'forecast',
  clean = FALSE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------
  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( is.null(modelRun) ) { # Use todays date if modelRun is null
    now <- lubridate::now(tzone = 'UTC')
    modelRun <- paste0(strftime(now, '%Y%m%d', tz = 'UTC'), '00')
  } else {
    # Make sure we end up with YYYYmmddHH
    modelRun <- as.character(modelRun)
    length <- stringr::str_length(modelRun)[1]
    if ( length == 8 ) { # if the length is 8 append 00 (model runs work YYYYmmddHH)
      modelRun <- paste0(modelRun, '00')
    }
  }
  # Verify YYYYmmddHH
  if ( !stringr::str_detect(modelRun, '[0-9]{10}') ) {
    stop("'modelRun' parameter must have 10 digits")
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

  # NOTE: CHECK LOGIC
  # Create a list of models to load parallel
  if ( (length(model) >= 1) && is.null(localPath) && (length(modelRun) <= 1) ) {
    for ( i in model ) {
      model_list[[i]] <- future::future({
        setModelDataDir(data_dir)
        ###bluesky_load(i, modelRun, xlim, ylim, localPath, baseUrl, modelType, clean, verbose)
        bluesky_load(
          model = i,
          modelRun = modelRun,
          baseUrl = baseUrl,
          modelType = modelType,
          localPath = localPath,
          xlim = xlim,
          ylim = ylim,
          clean = clean,
          verbose = verbose
        )
      })
    }
    .load_check(model_list[[1]], paste0('Loading Model: ', paste(model, collapse = ', ')), verbose)
  } else if ( length(localPath) >= 1 ) { # Check if to load multiple local files
    for ( i in localPath ) {
      model_list[[i]] <- future::future({
        setModelDataDir(data_dir)
        ###bluesky_load(model, modelRun, xlim, ylim, i, baseUrl, modelType, clean, verbose)
        bluesky_load(
          model = model,
          modelRun = modelRun,
          baseUrl = baseUrl,
          modelType = modelType,
          localPath = i,
          xlim = xlim,
          ylim = ylim,
          clean = clean,
          verbose = verbose
        )
      })
    }
    .load_check(model_list[[1]], paste0('Loading Local Files: ', paste(localPath, collapse = ', ')), verbose)
  } else if ( length(modelRun) >= 1 ) {
    for ( i in modelRun ) {
      model_list[[paste0(model,"_",i)]] <- future::future({
        setModelDataDir(data_dir)
        ###bluesky_load(model, i, xlim, ylim, localPath, baseUrl, modelType, clean, verbose)
        bluesky_load(
          model = model,
          modelRun = i,
          baseUrl = baseUrl,
          modelType = modelType,
          localPath = localPath,
          xlim = xlim,
          ylim = ylim,
          clean = clean,
          verbose = verbose
        )
      })
    }
    .load_check(model_list[[1]], paste0('Loading Model Runs: ', paste(modelRun, collapse = ', ')), verbose)
  } else {
    stop('Check parameters.')
  }

  models <- future::values(model_list)
  parallel::stopCluster(cl)

  return(models)

}

# ===== Internal Functions =====================================================

#' @keywords internal
#'
#' @title Show loading for futures
#' @param f a future
#' @param msg a message to display
#' @param verbose logical. to display
#'
#' @return NULL
.load_check <- function(f, msg, verbose) {
  if ( verbose ) {
    message(msg)
    while( !future::resolved(f) ) {
      message(".", appendLF = FALSE)
      Sys.sleep(1.5)
    }
    message('')
  }
  NULL
}


