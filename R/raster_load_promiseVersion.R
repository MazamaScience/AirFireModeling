#' #' @title Load BlueSky model data
#' #'
#' #' @param model Model identifier.
#' #' @param modelRun Model initialization datestamp as "YYYYMMDDHH".
#' #' @param modelType Subdirectory path containing BlueSky output, i.e. 'forcast'.
#' #' @param baseUrl Base URL for BlueSky output.
#' #' @param xlim A vector of coordinate longitude bounds.
#' #' @param ylim A vector of coordinate latitude bounds.
#' #' @param clean Logical specifying removal of original model data after conversion
#' #' to "v2" format.
#' #' @param verbose Logical to display messages.
#' #'
#' #' @description Load or download models and automatically convert NetCDF (.nc)
#' #' format to a spatio-temporal \code{Raster*} object.
#' #'
#' #' @details \code{model} and \code{modelRun} are vectorised parameters. If
#' #' multiple datasets need to be downloaded, this will be performed in parallel
#' #' using the \pkg{future} package.
#' #'
#' #' @return A list of one or more raster objects.
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(AirFireModeling)
#' #'
#' #' setModelDataDir('~/Data/BlueSky')
#' #'
#' #' # Load from server
#' #' rasterList <- raster_load(
#' #'   model = "PNW-4km",
#' #'   modelRun = c(2019100900, 2019101000, 2019101100),
#' #'   xlim = c(-125, -115),
#' #'   ylim = c(42, 50)
#' #' )
#' #'
#' #'
#' #' raster_map(rasterList[[1]], index = 3)
#' #' }
#' raster_load <- function(
#'   model = 'PNW-4km',
#'   modelRun = NULL,
#'   modelType = 'forecast',
#'   baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
#'   xlim = NULL,
#'   ylim = NULL,
#'   clean = TRUE,
#'   verbose = TRUE
#' ) {
#'
#'   # ----- Validate parameters --------------------------------------------------
#'
#'   MazamaCoreUtils::stopIfNull(baseUrl)
#'
#'   if ( is.null(modelRun) ) {
#'     # Use todays date if modelRun is null
#'     now <- lubridate::now(tzone = 'UTC')
#'     modelRun <- paste0(strftime(now, '%Y%m%d', tz = 'UTC'), '00')
#'   } else {
#'     # Make sure we end up with YYYYmmddHH
#'     modelRun <- as.character(modelRun)
#'     length <- stringr::str_length(modelRun)[1]
#'     if ( length == 8 ) { # if the length is 8 append 00 (model runs work YYYYmmddHH)
#'       modelRun <- paste0(modelRun, '00')
#'     }
#'   }
#'
#'   # Verify YYYYmmddHH
#'   if ( !stringr::str_detect(modelRun, '[0-9]{10}') ) {
#'     stop("'modelRun' parameter must have 10 digits")
#'   }
#'
#'   if ( length(modelType) > 1 )
#'     stop("Only a single 'modelType' can be used in each call to raster_load().")
#'
#'   # Defaults
#'   if ( !is.logical(clean) ) clean <- FALSE
#'   if ( !is.logical(verbose) ) verbose <- TRUE
#'
#'   # ----- Load data ------------------------------------------------------------
#'
#'   # Empty list of promises
#'   dataPromiseList <- list()
#'
#'   # NOTE:  We need to create all combinations of model and modelRun for
#'   # NOTE:  downloading. The expand.grid() function does just that.
#'
#'   # Create combinations
#'   allModelsDF <- expand.grid(model = model, modelRun = modelRun)
#'
#'   # Create a Parallel Socket Cluster
#'   cl <- parallel::makeCluster(future::availableCores() - 1)
#'   future::plan(strategy = future::cluster, workers = cl)
#'
#'   # NOTE:  Parallel operation requires that package environment variables like
#'   # NOTE:  modelDataDir need to be declared in each process.
#'
#'   modelDataDir <- getModelDataDir()
#'
#'   for ( i in seq_len(nrow(allModelsDF)) ) {
#'
#'     model <- allModelsDF$model[i]
#'     modelRun <- allModelsDF$modelRun[i]
#'     name <- sprintf("%s_%s", model, modelRun)
#'
#'     # Create "promises" to load data
#'     dataPromiseList[[name]] <- future::future({
#'       setModelDataDir(modelDataDir)
#'       bluesky_load(
#'         model = model,
#'         modelRun = modelRun,
#'         modelType = modelType,
#'         baseUrl = baseUrl,
#'         localPath = NULL,
#'         xlim = xlim,
#'         ylim = ylim,
#'         clean = clean,
#'         verbose = verbose
#'       )
#'     })
#'
#'     .load_check(dataPromiseList[[modelRun]],
#'                 sprintf("Loading %s_%s", model, modelRun),
#'                 verbose)
#'
#'   }
#'
#'   # Retrieve promised data
#'   modelList <- future::values(dataPromiseList)
#'   parallel::stopCluster(cl)
#'
#'   return(modelList)
#'
#' }
#'
#' # ===== Internal Functions =====================================================
#'
#' #' @keywords internal
#' #'
#' #' @title Show loading for futures
#' #' @param f a future
#' #' @param msg a message to display
#' #' @param verbose logical. to display
#' #'
#' #' @return NULL
#' .load_check <- function(f, msg, verbose) {
#'   if ( verbose ) {
#'     message(msg)
#'     while( !future::resolved(f) ) {
#'       message(".", appendLF = FALSE)
#'       Sys.sleep(1.5)
#'     }
#'     message('')
#'   }
#'   NULL
#' }
#'
#'
