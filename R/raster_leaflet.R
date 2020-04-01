#' @export
#' @title Raster Leaflet Map
#'
#' @param raster A \code{Raster\*} object.
#' @param index The index of the \code{Raster*} object. See details.
#' @param palette The color palette used to map cell values. This must be one
#' of the palettes available through \code{ggplot2::scale_colour_brewer()}.
#' @param breaks The breaks used to define the map cell color or \code{'default'}. CURRENTLY UNUSED
#' @param title (Optional) A plot title. CURRENTLY UNUSED
#' @param direction Numeric. \code{direction = -1} reverses color palette.
#' @param timezone The timezone to parse the dates. Default is UTC.
#' @param compare An optional comparison \code{Raster*} object
#' @param index2 The index of the comparison \code{Raster*} object.
#'
#' @description Create a leaflet interactive map using a model Raster Image.
#'
#' @details The \code{index} is typically associated with a time-axis or RasterLayer,
#' e.g \code{index = 1} is the first hour of a model.
#'
#' @return a leaflet map
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load consecutive model runs
#' rasterList <- raster_load(
#'   model = "PNW-4km",
#'   modelRun = c(2019100900, 2019101000),
#'   xlim = c(-125, -117),
#'   ylim = c(42, 47)
#' )
#'
#' # Create a leaflet map
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 3,
#'   title = "PNW-4km"
#' )
#'
#' # Create a leaflet map comparing two times from the same run
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 15,
#'   compare = rasterList[[1]],
#'   index2 = 18,
#'   title = "PNW-4km",
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Create a leaflet map comparing the same time from two different runs
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 27,
#'   compare = rasterList[[2]],
#'   index2 = 3,
#'   title = "PNW-4km",
#'   timezone = "America/Los_Angeles"
#'   )
#' }
raster_leaflet <- function(
  raster,
  index = 1,
  palette = 'Spectral', # 'Greys',
  breaks = 'default',
  title = 'PM2.5',
  direction = -1,
  timezone = 'UTC',
  compare = NULL,
  index2 = 1
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)
  if ( !grepl('[Rr]aster', class(raster)) )
    stop("Parameter 'raster' is not a `Raster*` object")

  if ( !is.numeric(index) )
    stop("Parameter 'index' must be numeric.")

  availablePalettes <- rownames(RColorBrewer::brewer.pal.info)
  if ( !palette %in% availablePalettes )
    stop(sprintf("'%s' is not a recognized palette. Please see ?ggplot2::scale_colour_brewer."))

  # breaks

  if ( !is.character(title) )
    stop("Parameter 'title' must be character.")

  if ( !direction %in% c(-1,1) )
    stop("Parameter 'direction' must be either 1 or -1.")

  if ( !timezone %in% OlsonNames())
    stop("Parameter 'timezone' is not recognized. Please see ?OlsonNames/.")

  if ( !is.numeric(index2) )
    stop("Parameter 'index2' must be numeric.")

  # ----- Prepare data ---------------------------------------------------------

  # Subset anything that has layers
  if ( raster::nlayers(raster) > 1 )
    raster <- raster[[index]]

  if ( !is.null(compare) ) {
    if ( raster::nlayers(compare) > 1 )
      compare <- compare[[index2]]
  }

  # Color palette function
  pal <- leaflet::colorNumeric(
    palette,
    raster::values(raster),
    reverse = ifelse(direction == -1, TRUE, FALSE),
    na.color = "transparent"
  )

  # ----- Create leaflet map ---------------------------------------------------

  if ( !is.null(compare) ) {

    leaf <-
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addRasterImage(
        raster,
        colors = pal,
        opacity = 0.8,
        group = .createLayerTimeString("A: ", names(raster), timezone)
      ) %>%
      leaflet::addRasterImage(
        compare,
        colors = pal,
        opacity = 0.8,
        group = .createLayerTimeString("B: ", names(compare), timezone)
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = raster::values(raster),
        title = 'PM2.5'
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c(
          .createLayerTimeString("A: ", names(raster), timezone),
          .createLayerTimeString("B: ", names(compare), timezone)
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )

    return(leaf)

  } else {

    leaf <-
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addRasterImage(
        raster,
        colors = pal,
        opacity = 0.8
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = raster::values(raster),
        title = 'PM2.5'
      )

    return(leaf)

  }

}

# ===== Internal Functions =====================================================

.createLayerTimeString <- function(id, layerName, timezone = "UTC") {
  epochSecs <- as.numeric(stringr::str_remove(layerName, 'X'))
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)
  timeString <- paste0(id, strftime(layerTime, format = "%Y-%m-%d %H:00 %Z", tz = timezone))
  return(timeString)
}

