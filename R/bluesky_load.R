#' @export
#' @title Load a BlueSky Model Raster
#'
#' @param filePath Absolute path of the local bluesky .nc file.
#' @param modelRun Date code as "YYYYMMDDHH" (integer or character).
#' @param parameter Parameter name.
#' @param cleanup Logical specifying whether to remove the original files.
#' @param baseUrl Base URL for BlueSky output.
#' @param dailyOutputDir BlueSky web directory.
#' @param model Model identifier.
#' @param subDir Subdirectory path containing netcdf data.
#' @param verbose If \code{TRUE}, suppress status messages (if any), and the
#' progress bar.
#'
#'#' @description Read and/or downloads a BlueSky NetCDF
#'
#' Data are searched for in the package internal \code{dataDir} which can be set
#' with \code{setModelDataDir()}.
#'
#' If \code{timesteps} are specified, the returned \emph{bs_grid} object will
#' contain data associated with those timesteps. Data missing in the original
#' model run will be replaced with NA values.
#'
#' @details
#'
#' On 2019-10-11, vailable model identifiers include the following:
#' \itemize{
#'   \item{AK-12km}
#'   \item{CANSAC-1.33km}
#'   \item{CANSAC-4km}
#'   \item{DRI1.33km-CMAQ}
#'   \item{DRI4km-CMAQ}
#'   \item{GFS-0.15deg-CanadaUSA-p25deg-68N}
#'   \item{GFS-0.15deg}
#'   \item{NAM-3km}
#'   \item{NAM84-0.15deg}
#'   \item{PNW-1.33km}
#'   \item{PNW-4km}
#'   \item{PNW1.33km-CMAQ}
#'   \item{PNW4km-CMAQ}
#' }
#'
#' @return List of class \code{bs_grid} with the following elements:
#' \itemize{
#'   \item{lon -- 1D vector of longitudes (X)}
#'   \item{lat -- 1D vector of latitudes (Y)}
#'   \item{time -- 1D vector of POSIXct times (T)}
#'   \item{data -- list of 3D arrays of data values (X-Y-T)}
#'   \item{model -- character name of model used to generate the data}
#'   \item{modelRun -- character date + hour when the model was run}
#'   \item{deltaLon -- longitude axis grid cell spacing}
#'   \item{deltaLat -- latitude axis grid cell spacing}
#' }
#'
#' BlueSky output files are found in directories with the following
#' structure:
#'
#' \preformatted{
#' .../standard/NAM84-0.08deg/2016050600/carryover/data/...
#' .../standard/NAM84-0.08deg/2016050600/combined/data/...
#' .../standard/NAM84-0.08deg/2016050600/forecast/data/...
#' }
#'
#' @seealso \link{setModelDataDir}
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bs <- bluesky_load(
#'   model = "PNW-1.33km",
#'   modelRun = 2019100900
#' )
#' }
bluesky_load <- function(
  filePath = NULL,
  modelRun= NULL,
  parameter = 'pm25',
  cleanup = TRUE,
  baseUrl = "https://haze.airfire.org/bluesky-daily/output",
  dailyOutputDir = "standard",
  model = 'CANSAC-1.33km',
  subDir = "forecast",
  xlim = NULL,
  ylim = NULL,
  verbose = TRUE
) {

  # Checks
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(dailyOutputDir)
  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(subDir)
  MazamaCoreUtils::stopIfNull(baseUrl)
  MazamaCoreUtils::stopIfNull(parameter)

  if ( is.null(modelRun) ) {
    now <- lubridate::now(tzone = "UTC")
    modelRun <- paste0(strftime(now, "%Y%m%d", tz = "UTC"), "00")
  } else {
    # Make sure we end up with YYYYmmddHH
    modelRun <- as.character(modelRun)
    length <- stringr::str_length(modelRun)[1]
    if ( length == 8 ) {
      modelRun <- paste0(modelRun,'00')
    }
  }

  # Verify YYYYmmddHH
  if ( !stringr::str_detect(modelRun, "[0-9]") ) {
    stop("'modelRun' parameter must have 10 digits")
  }

  parameter <- tolower(parameter)
  if ( parameter != "pm25" ) {
    stop(paste0(
      "Parameter '", parameter, "' is not handled yet."
    ))
  }

  # Default to cleanup
  if ( !is.logical(cleanup) )
    cleanup <- TRUE

  # Default to verbose
  if ( !is.logical(verbose) )
    verbose <- TRUE

  cl <- parallel::makeCluster(future::availableCores() - 1)
  future::plan(strategy = future::cluster, workers = cl)
  model_dir <- getModelDataDir()

  model_list <- list()

  for ( i in model ) {
    model_list[[i]] <- future::future({
      setModelDataDir(model_dir)
      model_brick <- .load_brick(filePath, i, modelRun, dailyOutputDir, subDir, baseUrl, verbose, cleanup, model_dir)

      # Handle Xlim and Ylim
      if ( !is.null(xlim) || !is.null(ylim) ) {

        if ( is.null(ylim) ) {
          ylim <- c(raster::ymin(model_brick), raster::ymax(model_brick))
        }
        if ( is.null(xlim) ) {
          xlim <- c(raster::xmin(model_brick), raster::xmax(model_brick))
        }

        # if ( abs(xlim[1]) < abs(raster::xmin(model_brick)) | abs(xlim[2]) > abs(raster::xmax(model_brick)) ) {
        #   stop('xlim out of coordinate domain.')
        # }

        cells <- raster::cellFromXY(model_brick, cbind(xlim, ylim))
        ext <- raster::extentFromCells(model_brick, cells)
        return(raster::crop(model_brick, ext))

      } else {
        return(model_brick)
      }

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


