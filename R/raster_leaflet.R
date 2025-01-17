#' @export
#' @title Raster Leaflet Map
#'
#' @param raster A Raster\* object.
#' @param index And index into the Raster\* object. See details.
#' @param palette Color palette used to map cell values. This must be either
#' \code{"aqi"} or one of the palette names available through
#' \code{ggplot2::scale_colour_brewer()}.
#' @param breaks The breaks used to map cell values to colors. CURRENTLY UNUSED
#' @param direction Numeric. \code{direction = -1} reverses color palette.
#' @param opacity Opacity of raster layer.
#' @param maptype Name of leaflet ProviderTiles to use.
#' @param title (Optional) A plot title. CURRENTLY UNUSED
#' @param timezone Olson timezone in which times will be displayed.
#' @param compare Optional comparison Raster\* object.
#' @param index2 Index of the comparison Raster\* object.
#'
#' @description Create a leaflet interactive map using a model output
#' Raster\* object.
#'
#' @details The \code{index} is associated with the z axis or time-axis of
#' \code{RasterBrick} object, e.g \code{index = 1} is the first hour of a model.
#'
#' The \code{maptype} argument is mapped onto leaflet "ProviderTile" names.
#' Current mappings include:
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#'
#' @return A leaflet map.
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load consecutive model runs
#' rasterList <- raster_load(
#'   modelName = "PNW-4km",
#'   modelRun = c(2020091300, 2020091400),
#'   xlim = c(-125, -117),
#'   ylim = c(42, 47)
#' )
#'
#' # Create a leaflet map
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 3,
#'   palette = "aqi",
#'   direction = 1
#' )
#'
#' # Create a leaflet map comparing two times from the same run
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 12,
#'   compare = rasterList[[1]],
#'   index2 = 36,
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Create a leaflet map comparing the same time from two different runs
#'
#' # Load consecutive model runs
#' rasterList <- raster_load(
#'   modelName = c("PNW-4km", "PNW-1.33km"),
#'   modelRun = 2020091300,
#'   xlim = c(-125, -117),
#'   ylim = c(42, 47)
#' )
#'
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 12,
#'   compare = rasterList[[2]],
#'   index2 = 12,
#'   timezone = "America/Los_Angeles",
#'   opacity = 0.3
#'   )
#' }

raster_leaflet <- function(
  raster,
  index = 1,
  palette = 'Spectral',
  breaks = c(0, 12, 35, 55, 150, 250, 350, Inf),
  direction = -1,
  opacity = 0.6,
  maptype = 'terrain',
  title = 'PM2.5',
  timezone = 'UTC',
  compare = NULL,
  index2 = 1
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)

  if ( !raster_isRaster(raster) )
    stop("Parameter 'raster' is not a `Raster*` object")

  if ( !is.numeric(index) )
    stop("Parameter 'index' must be numeric.")

  brewerPalettes <- rownames(RColorBrewer::brewer.pal.info)
  otherPalettes <- c('aqi') # from PWFSLSmoke
  availablePalettes <- union(brewerPalettes, otherPalettes)
  if ( !palette %in% availablePalettes )
    stop(sprintf("'%s' is not a recognized palette. Please see ?ggplot2::scale_colour_brewer."))

  if ( !is.numeric(breaks) )
    stop("Parameter 'breaks' must be a numeric vector.")

  if ( !direction %in% c(-1,1) )
    stop("Parameter 'direction' must be either 1 or -1.")

  if ( !is.numeric(opacity) || opacity < 0 || opacity > 1 )
    stop("Parameter 'opacity' must be numeric between [0, 1].")

  if ( !is.character(title) )
    stop("Parameter 'title' must be character.")

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

  # # TODO:  Use Binned color palette function?
  # pal <- leaflet::colorBin(
  #   palette,
  #   raster::values(raster),
  #   bins = breaks,
  #   reverse = ifelse(direction == -1, TRUE, FALSE),
  #   na.color = "transparent"
  # )

  # Color palette function
  if ( palette == "aqi" ) {
    pal <- leaflet::colorBin(
      palette = PWFSLSmoke::AQI$colors,
      bins = PWFSLSmoke::AQI$breaks_24,
      reverse = ifelse(direction == -1, TRUE, FALSE),
      na.color = "transparent"
    )
  } else {
    pal <- leaflet::colorNumeric(
      palette,
      raster::values(raster),
      reverse = ifelse(direction == -1, TRUE, FALSE),
      na.color = "transparent"
    )
  }

  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == "terrain") {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }

  # ----- Create leaflet map ---------------------------------------------------

  if ( !is.null(compare) ) {

    leaf <-
      leaflet::leaflet() %>%
      ###leaflet::addTiles() %>%
      leaflet::addProviderTiles(providerTiles) %>%
      leaflet::addRasterImage(
        raster,
        colors = pal,
        opacity = opacity,
        group = paste0("A: ", raster_getTime(raster, timezone)),
        method = "ngb"
      ) %>%
      leaflet::addRasterImage(
        compare,
        colors = pal,
        opacity = opacity,
        group = paste0("B: ", raster_getTime(raster, timezone)),
        method = "ngb"
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = raster::values(raster),
        title = 'PM2.5'
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c(
          paste0("A: ", raster_getTime(raster, timezone)),
          paste0("B: ", raster_getTime(raster, timezone))
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )

    return(leaf)

  } else {

    leaf <-
      leaflet::leaflet() %>%
      ###leaflet::addTiles() %>%
      leaflet::addProviderTiles(providerTiles) %>%
      leaflet::addRasterImage(
        raster,
        colors = pal,
        opacity = opacity,
        method = "ngb"
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = raster::values(raster),
        title = 'PM2.5'
      )

    return(leaf)

  }

}
