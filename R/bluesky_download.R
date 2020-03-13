#' @export
#' @title Download BlueSky model data from AirFire
#'
#' @param dailyOutputDir BlueSky web directory
#' @param model Model identifier.
#' @param modelRun Date code as "YYYYMMDDHH".
#' @param subDir Subdirectory path containing netcdf data.
#' @param baseUrl Base URL for BlueSky output.
#' @param verbose If \code{FALSE}, suppress status messages (if any), and the
#' progress bar.
#'
#' @description Downloads a copy of the specified BlueSky model run to the
#' package data directory. This file can then be converted into a more
#' modernized format by assimilation.
#'
#' #' On 2019-10-11, vailable model identifiers include the following:
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
#' Users will typically call bluesky_load() which in turn calls this function.
#'
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
#' @return File path of downloaded data.
#'
#' @seealso \link{setModelDataDir}
#' @examples
#' \dontrun{
#' setModelDataDir("~/Data/Bluesky")
#' bluesky_download(model = "PNW-4km", modelRun = 2019100900)
#' }

bluesky_download <- function(
  model = 'PNW-4km',
  run = NULL,
  dirURL = 'https://haze.airfire.org/bluesky-daily/output/standard',
  type = 'forecast',
  verbose = TRUE
  ) {
  # ----- Validate parameters --------------------------------------------------
#
#   MazamaCoreUtils::stopIfNull(dailyOutputDir)
#   MazamaCoreUtils::stopIfNull(model)
#   MazamaCoreUtils::stopIfNull(modelRun)
#   MazamaCoreUtils::stopIfNull(subDir)
#   MazamaCoreUtils::stopIfNull(baseUrl)

  # Just in case
  if ( length(model) > 1 || length(run) > 1 ) {
    warning(paste0(
      "'model' or 'run' has multiple values -- ",
      "first value being used."
    ))
    model <- model[1]
    run <- as.character(run[1])
  }

  # Verify YYYYmmddHH
  if ( !stringr::str_detect(run, "[0-9]{10}") ) {
    stop("'modelRun' parameter must have 10 digits")
  }

  # Default to verbose
  if ( !is.logical(verbose) )
    verbose <- TRUE

  # ----- Create URL -----------------------------------------------------------
  # Create directory URL
  dir_url <- paste0( dirURL, "/",
                 model, "/",
                 run, "/",
                 ifelse(is.null(type), NULL, paste0(type, "/")) )


  # NOTE: Detect bluesky output version via summary.json
  detect_version <- function(dir_url) {
    content <- readLines(dir_url)
    if ( any(stringr::str_detect(content, 'summary.json')) ) {
      summary <- jsonlite::fromJSON(paste0(dir_url, 'summary.json'))
      version <- summary$output_version
    } else {
      stop('Invalid Directory: Cannot parse BlueSky output version.')
    }
    if ( verbose ) {
      message(paste0('Auto-detected ', model, ' BlueSky Output Version: ', version))
    }
    return(version)
  }

  fileName <- paste0(model, "_", run, ".nc")
  filePath <- file.path(getModelDataDir(), fileName)

  # ----- Download data --------------------------------------------------------
  if ( !file.exists(filePath) ) {
    version <- detect_version(dir_url)

    # Use detected bluesky output version to construct .nc file url
    if ( version == '1.0.0' ) {
      fileUrl <- paste0(dir_url, 'data/smoke_dispersion.nc')
      tryCatch(
        expr = {
          utils::download.file(url = fileUrl,  destfile = filePath, quiet = !verbose)
        },
        error = function(e) {
          stop(paste0('Error downloading: ', model))
        }
      )
    } else if ( version == '2.0.0' ) {
      fileUrl <- paste0(dir_url, 'hysplit_conc.nc')
      tryCatch(
        expr = {
          utils::download.file(url = fileUrl, destfile = filePath, quiet = !verbose)
        },
        error = function(e) {
          stop(paste0('Error downloading: ', model))
        }
      )
    } else {
      stop('Error Downloadind: Invalid BlueSky output version')
    }

  } else {
    message(paste0('BlueSky Model Exists at: ', filePath))
    message('Loading ...')
  }

  # ----- Return ---------------------------------------------------------------

  return(filePath)

}
