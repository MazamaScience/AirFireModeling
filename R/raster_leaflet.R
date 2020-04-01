#' @export
#' @title Raster Leaflet Map
#'
#' @param raster A \code{Raster\*} object.
#' @param index And index into the \code{Raster*} object. See details.
#' @param palette Color palette used to map cell values. This must be one
#' of the palettes available through \code{ggplot2::scale_colour_brewer()}.
#' @param breaks The breaks used to map cell values to colors. CURRENTLY UNUSED
#' @param direction Numeric. \code{direction = -1} reverses color palette.
#' @param opacity Opacity of raster layer.
#' @param maptype Name of leaflet ProviderTiles to use.
#' @param title (Optional) A plot title. CURRENTLY UNUSED
#' @param timezone Olson timezone in which times will be displayed.
#' @param compare Optional comparison \code{Raster*} object.
#' @param index2 Index of the comparison \code{Raster*} object.
#'
#' @description Create a leaflet interactive map using a model output
#' \code{Raster\*} object.
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
#'   model = "PNW-4km",
#'   modelRun = c(2019100900, 2019101000),
#'   xlim = c(-125, -117),
#'   ylim = c(42, 47)
#' )
#'
#' # Create a leaflet map
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 3
#' )
#'
#' # Create a leaflet map comparing two times from the same run
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 15,
#'   compare = rasterList[[1]],
#'   index2 = 18,
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Create a leaflet map comparing the same time from two different runs
#'
#' # Load consecutive model runs
#' rasterList <- raster_load(
#'   model = c("PNW-4km", "PNW-1.33km"),
#'   modelRun = 2019100900,
#'   xlim = c(-125, -117),
#'   ylim = c(42, 47)
#' )
#'
#' raster_leaflet(
#'   raster = rasterList[[1]],
#'   index = 15,
#'   compare = rasterList[[2]],
#'   index2 = 15,
#'   timezone = "America/Los_Angeles",
#'   opacity = 0.3
#'   )
#' }
raster_leaflet <- function(
  raster,
  index = 1,
  palette = 'Spectral', # 'Greys',
  breaks = c(0, 12, 35, 55, 150, 250, 350, 500),
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
  if ( !grepl('[Rr]aster', class(raster)) )
    stop("Parameter 'raster' is not a `Raster*` object")

  if ( !is.numeric(index) )
    stop("Parameter 'index' must be numeric.")

  availablePalettes <- rownames(RColorBrewer::brewer.pal.info)
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
  pal <- leaflet::colorNumeric(
    palette,
    raster::values(raster),
    reverse = ifelse(direction == -1, TRUE, FALSE),
    na.color = "transparent"
  )

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
        group = createLayerTimeString(names(raster), timezone, prefix = "A: ")
      ) %>%
      leaflet::addRasterImage(
        compare,
        colors = pal,
        opacity = opacity,
        group = createLayerTimeString(names(compare), timezone, prefix = "B: ")
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = raster::values(raster),
        title = 'PM2.5'
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c(
          createLayerTimeString(names(raster), timezone, prefix = "A: "),
          createLayerTimeString(names(compare), timezone, prefix = "B: ")
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
        opacity = opacity
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = raster::values(raster),
        title = 'PM2.5'
      )

    return(leaf)

  }

}
