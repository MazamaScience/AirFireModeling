#' @title List downloaded models
#'
#' @param modelDataDir Directory in which models are located.
#' @param pattern A regex pattern to use for filtering model files.
#' @param full Logical. Show the full path of the model (used for local loading).
#' @param ... Additional arguments to be passed to \code{list.files()}.
#'
#' @return A list of downloaded models
#' @export
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' filePath <- bluesky_download(modelName = "PNW-4km", modelRun = 2019100900)
#' bluesky_toCommonFormat(filePath)
#' bluesky_downloaded()
#' }
#'
bluesky_downloaded <- function(
  modelDataDir = getModelDataDir(),
  pattern = '.nc',
  full = FALSE,
  ...
) {
  list.files(
    path = modelDataDir,
    full.names = full,
    no.. = TRUE,
    pattern = pattern,
    ...
  )
}
