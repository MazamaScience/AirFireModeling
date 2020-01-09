#' @export
#' @title Small Multiples of Raster
#'
#' @param raster a Raster* object
#' @param breaks Color break locations.
#' @param palette A color brewer palette (i.e. 'Greys', 'Blues', 'BuGn', 'YlOrRd', ...).
#' @param direction Color palette direction. Set -1 to reverse direction.
#' @param timezone Timezone.
#'
#' @return a gg object
raster_facet <- function(
  raster,
  breaks = 'default',
  palette = 'default',
  direction = 1,
  timezone = 'UTC'
) {

  # TODO: Add Proper checks

  # Function to turn numeric POSIX time to character string
  t2str <- function(s) {
    x <- as.numeric(stringr::str_remove(s, 'X'))
    class(x) <- c('POSIX', 'POSIXct')
    attr(x, 'tzone') <- timezone
    return(as.character.Date(x))
  }

  # Define color breaks
  if ( breaks == 'default' ) {
    breaks <- c(0,12, 35, 55, 150, 250, 350, 500)
  }

  # Determine color scaler
  if (palette == 'default' ) {
    scale_colors <-  ggplot2::scale_fill_viridis_d( option = 'A',direction = direction )} else {
      scale_colors <-              ggplot2::scale_fill_brewer( na.value = 'white', palette = palette, direction = direction )}

  # Facet Plot
  gg <- rasterVis::gplot(raster) +
    ggplot2::geom_raster(ggplot2::aes(fill = cut(.data$value, breaks = breaks))) +
    ggplot2::facet_wrap( ~.data$variable,
                         labeller = ggplot2::labeller(.default=t2str) ) +
    scale_colors +
    ggplot2::scale_fill_viridis_d(option = 'A', direction = direction) +
    ggplot2::labs(x = 'Longitude', y = 'Latitude', fill = 'PM2.5') +
    ggplot2::theme_classic()

  return(gg)
}
