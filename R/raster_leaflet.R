#' @export
#' @title Raster Leaflet Map
#'
#' @param raster A Raster* object
#' @param index The index of the Raster* object. See details.
#' @param compare An optional comparison Raster
#' @param palette A color palette, default is Spectral
#' @param direction Color palette direction. -1 is reverse.
#' 
#' @description Create a leaflet interactive map using a model Raster Image.
#' 
#' @details The \code{index} is typically associated with a time-axis or RasterLayer,
#' e.g \code{index = 1} is the first hour of a model.
#'
#' @return a leaflet map
#' @examples 
#' \dontrun{
#' bs <- raster_load()
#' # Show 6th hour
#' raster_leaflet(bs, index = 6)
#' }
raster_leaflet <- function(
  raster,
  index = 1,
  compare = NULL,
  palette = 'Spectral',
  direction = -1
) {

  if ( !grepl('[Rr]aster', class(raster)) ) {
    stop('Raster* not a `Raster*` object')
  }

  if ( raster::nlayers(raster) || raster::nlayers(compare) ) {
    raster <- raster[[index]]
    compare <- compare[[index]]
  }

  t2str <- function(s) {
    x <- as.numeric(stringr::str_remove(s, 'X'))
    class(x) <- c('POSIX', 'POSIXct')
    attr(x, 'tzone') <- 'UTC'
    return(as.character.Date(x))
  }

  pal <- leaflet::colorNumeric( palette,
                                raster::values(raster),
                                reverse = ifelse(direction == -1, TRUE, FALSE),
                                na.color = "transparent" )

    if ( !is.null(compare) ) {

      leaf <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addRasterImage( raster,
                                colors = pal,
                                 opacity = 0.8,
                                 group = t2str(names(raster)) ) %>%
        leaflet::addRasterImage( compare,
                                 colors = pal,
                                 opacity = 0.8,
                                 group = t2str(names(compare)) ) %>%
        leaflet::addLegend( pal = pal,
                            values = raster::values(raster),
                            title = 'PM2.5' ) %>%
        leaflet::addLayersControl( baseGroups = c( t2str(names(raster)),
                                                   t2str(names(compare)) ),
                                   options = leaflet::layersControlOptions(collapsed = FALSE) )

      return(leaf)

    } else {

      leaf <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addRasterImage( raster,
                                 colors = pal,
                                 opacity = 0.8 ) %>%
        leaflet::addLegend( pal = pal,
                            values = raster::values(raster),
                            title = 'PM2.5' )

      return(leaf)

    }

}
