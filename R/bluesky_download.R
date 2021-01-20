#' @export
#' @title Download BlueSky model data from AirFire
#'
#' @param modelName Model identifier.
#' @param modelRun Model initialization datestamp as "YYYYMMDDHH".
#' @param modelMode Subdirectory path containing BlueSky output, i.e. 'forcast'.
#' @param baseUrl Base URL for BlueSky output.
#' @param verbose If \code{FALSE}, suppress status messages (if any), and the
#' progress bar.
#'
#' @description Downloads a copy of the specified BlueSky model run to the
#' package data directory. This file can then be converted into a common format.
#'
#' #' On 2019-10-11, available model identifiers include the following:
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
#' BlueSky output files are found in directories with the following
#' structure: \code{<baseUrl>/<modelName>/<modelRun>/<modelMode>/data/...}
#'
#' \preformatted{
#' <baseUrl>/NAM84-0.08deg/2016050600/carryover/data/...
#' <baseUrl>/NAM84-0.08deg/2016050600/combined/data/...
#' <baseUrl>/NAM84-0.08deg/2016050600/forecast/data/...
#' }
#'
#' @return File path of downloaded data.
#'
#' @seealso \link{setModelDataDir}
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' filePath <- bluesky_download(modelName = "PNW-4km", modelRun = 2019100900)
#' bluesky_toCommonFormat(filePath)
#' bluesky_downloaded()
#' }

bluesky_download <- function(
  modelName = 'PNW-4km',
  modelRun = NULL,
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  modelMode = 'forecast',
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(modelName)
  MazamaCoreUtils::stopIfNull(modelRun)
  MazamaCoreUtils::stopIfNull(baseUrl)
  MazamaCoreUtils::stopIfNull(modelMode)

  # Just in case
  if ( length(modelName) > 1 || length(modelRun) > 1 ) {
    warning(paste0(
      "'modelName' or 'modelRun' has multiple values -- ",
      "first value being used."
    ))
    modelName <- modelName[1]
    modelRun <- as.character(modelRun[1])
  }

  # Verify YYYYmmddHH
  if ( !stringr::str_detect(modelRun, "[0-9]{10}") ) {
    stop("'modelRun' parameter must have 10 digits")
  }

  # Defaults
  if ( !is.logical(verbose) ) verbose <- TRUE

  # ----- Create URL, name and path---------------------------------------------

  # Create directory URL
  dataDirUrl <- paste0(
    baseUrl, "/",
    modelName, "/",
    modelRun, "/",
    ifelse(is.null(modelMode), NULL, paste0(modelMode, "/"))
  )

  fileName <- paste0(modelName, "_", modelRun, ".nc")
  filePath <- file.path(getModelDataDir(), fileName)

  # ----- Download data --------------------------------------------------------

  if ( file.exists(filePath) ) {

    if ( verbose )
      message(paste0('BlueSky Model Exists at: ', filePath))

  } else {

    # NOTE: Detect bluesky output version via summary.json

    # * detect model version -----

    content <- readLines(dataDirUrl)
    if ( any(stringr::str_detect(content, 'summary.json')) ) {
      summary <- jsonlite::fromJSON(paste0(dataDirUrl, 'summary.json'))
      version <- summary$output_version
    } else {
      stop(sprintf(
        "Cannot determine model version\nNo 'summary.json' file at %s",
        dataDirUrl
      ))
    }

    if ( verbose )
      message(paste0('Auto-detected ', modelName, ' BlueSky Output Version: ', version))

    # * downlaod .nc file -----

    if ( stringr::str_detect(version, "1\\.") ) {

      fileUrl <- paste0(dataDirUrl, 'data/smoke_dispersion.nc')
      tryCatch(
        expr = {
          utils::download.file(url = fileUrl, destfile = filePath, quiet = !verbose)
        },
        error = function(e) {
          stop(paste0("Error downloading: ", modelName))
        }
      )

    } else if ( stringr::str_detect(version, "2\\.") ) {

      fileUrl <- paste0(dataDirUrl, 'hysplit_conc.nc')
      tryCatch(
        expr = {
          utils::download.file(url = fileUrl, destfile = filePath, quiet = !verbose)
        },
        error = function(e) {
          stop(paste0("Error downloading: ", modelName))
        }
      )

    } else {

      stop(paste0("Error downloading: Invalid BlueSky output version: ", version))

    }

  } # END of file doesn't exist

  # ----- Return ---------------------------------------------------------------

  return(filePath)

}
