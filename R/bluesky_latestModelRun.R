#' @export
#' @title Find latest BlueSky model run from AirFire
#'
#' @param modelName Model identifier.
#' @param count Number of recent \code{modelRun} strings to return.
#' @param baseUrl Base URL for BlueSky output.
#' @param verbose If \code{FALSE}, suppress status messages (if any), and the
#' progress bar.
#'
#' @description Scans the directory of BluSky model output and returns the
#' most recent \code{modelRun} string(s). If \code{count > 1}, the most recent
#' \code{count} will be returned in low-hi order.
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
#' Users wanting the latest data will typically call this function first,
#' followed by bluesky_load().
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
#' @return Timestamp of the latest model run.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' modelRun <- bluesky_latestModelRun(modelName = "PNW-4km")
#' filePath <- bluesky_download(modelName = "PNW-4km", modelRun = modelRun)
#' bluesky_toCommonFormat(filePath)
#' bluesky_downloaded()
#' }

bluesky_latestModelRun <- function(
  modelName = NULL,
  count = 1,
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(modelName)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # Just in case
  if ( length(modelName) > 1 ) {
    warning(paste0(
      "'modelName' has multiple values -- ",
      "first value being used."
    ))
    modelName <- modelName[1]
  }

  # TODO:  Should we validate the model name against bluesky_modelInfo?

  # Defaults
  if ( !is.logical(verbose) )
    verbose <- TRUE

  # ----- Download modelRun datestamps -----------------------------------------

  # Create directory URL
  dataDirUrl <- paste0(
    baseUrl, "/",
    modelName
  )

  # TODO:  Put this internet-dependent chunk in a try block and handle errors

  # Check remotely for available files and build filelist
  links <-
    xml2::read_html(dataDirUrl) %>%
    xml2::xml_child("body") %>%
    xml2::xml_child("table") %>%
    xml2::xml_find_all("//a") %>%
    xml2::xml_attr("href")

  modelRun <-
    links %>%
    stringr::str_replace("/","") %>%
    stringr::str_subset("^[0-9]{10}$") %>%
    utils::tail(count) %>%
    unique() %>%
    sort()

  # TODO:  Validate that modelRun ends up with an appropriate timestamp.
  # TODO:  Otherwise return NA or stop with a "No timestamps found" error.

  # ----- Return ---------------------------------------------------------------

  return(modelRun)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  modelName = 'PNW-4km'
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard'
  verbose = TRUE

}
