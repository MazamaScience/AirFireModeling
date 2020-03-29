#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @title Find models covering a location
#' @description Searches \code{bluesky_modelInfo} for all models whose domain
#' includes the incoming location.
#'
#' @param longitude the target longitude
#' @param latitude the target latitude
#'
#' @return Vector of model names.
bluesky_findModels <- function(
  longitude = NULL,
  latitude = NULL
) {

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)

  # Use the information found in bluesky_modelInfo
  models <-
    dplyr::filter(
      bluesky_modelInfo,
      longitude >= .data$MIN_LONGITUDE &
        longitude <= .data$MAX_LONGITUDE &
        latitude >= .data$MIN_LATITUDE &
        latitude <= .data$MAX_LATITUDE
    ) %>%
    dplyr::pull(.data$model)

  return(models)

}
