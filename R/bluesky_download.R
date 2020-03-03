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
  dailyOutputDir = "standard",
  model = "PNW-1.33km",
  modelRun = NULL,
  version = "3.5",
  subDir = "combined",
  baseUrl = "https://haze.airfire.org/bluesky-daily/output",
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(dailyOutputDir)
  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(modelRun)
  MazamaCoreUtils::stopIfNull(subDir)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # Just in case
  if ( length(model) > 1 || length(modelRun) > 1 ) {
    warning(paste0(
      "'model' or 'modelRun' has multiple values -- ",
      "first value being used."
    ))
    model <- model[1]
    modelRun <- as.character(modelRun[1])
  }

  # Verify YYYYmmddHH
  if ( !stringr::str_detect(modelRun, "[0-9]{10}") ) {
    stop("'modelRun' parameter must have 10 digits")
  }

  # Default to verbose
  if ( !is.logical(verbose) )
    verbose <- TRUE

  # ----- Create URL -----------------------------------------------------------

  # NOTE: Check version. BlueSky Version 4.1 requires slightly different naming

    if ( is.null(subDir) ) {
      if ( version == "4.1" ) {

        fileURL <- paste0( baseUrl, "/",
                           dailyOutputDir, "/",
                           model, "/",
                           modelRun, "/",
                           "hysplit_conc.nc" ) # NOTE: Not in data/ dir

      } else if ( version == "3.5" ) {

        fileURL <- paste0( baseUrl, "/",
                           dailyOutputDir, "/",
                           model, "/",
                           modelRun, "/",
                          "data/smoke_dispersion.nc" )

      } else {
        stop("Invalid BlueSky version.")
      }

    } else {

      if ( version == "4.1" ) {

        fileURL <- paste0( baseUrl, "/",
                           dailyOutputDir, "/",
                           model, "/",
                           modelRun, "/",
                           subDir, "/",
                           "hysplit_conc.nc" ) # NOTE: Not in data/ dir

      } else if ( version == "3.5" ) {

        fileURL <- paste0( baseUrl, "/",
                           dailyOutputDir, "/",
                           model, "/",
                           modelRun, "/",
                           subDir, "/",
                           "data/smoke_dispersion.nc" )

      } else {
        stop("Invalid BlueSky version.")
      }

    }

    fileName <- paste0(model, "_", modelRun, ".nc")
    filePath <- file.path(getModelDataDir(), fileName)


  # ----- Download data --------------------------------------------------------

  if ( !file.exists(filePath) ) {

    result <- try({
      utils::download.file(url = fileURL, destfile = filePath, quiet = !verbose)
    }, silent = FALSE)

    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( stringr::str_detect(err_msg,"cannot open destfile") ) {
        # Option to stop with a different message
      } else if ( stringr::str_detect(err_msg,"404 Not Found") ) {
        # Option to stop with a different message
      } else {
        # Option to stop with a different message
      }
    }

  }

  # ----- Return ---------------------------------------------------------------

  return(filePath)

}
