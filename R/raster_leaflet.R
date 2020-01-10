#' @export
#' @title Raster Leaflet Map
#'
#' @param raster A Raster* object
#' @param palette A color palette, default is Spectral
#' @param direction Color palette direction. -1 is reverse.
#'
#' @return a leaflet map
raster_leaflet <- function(raster, palette = 'Spectral', direction = -1) {

  # TODO: Add checks

  pal <- leaflet::colorNumeric( palette,
                                raster::values(raster),
                                reverse = ifelse(direction == -1, TRUE, FALSE),
                                na.color = "transparent" )

  leaf <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addRasterImage(raster, colors = pal, opacity = 0.8) %>%
    leaflet::addLegend(pal = pal, values = raster::values(raster), title = 'PM2.5')

  return(leaf)
}
