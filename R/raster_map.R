#' @export
#' @title Plot a Raster Map
#'
#' @param raster A Raster* object.
#' @param palette A color brewer palette (i.e. 'Greys', 'Blues', 'BuGn', 'YlOrRd', ...).
#' @param breaks Color break locations.
#' @param title A plot title.
#'
#' @return a ggobject
raster_map <-
  function(
    raster,
    palette = 'Greys',
    breaks = 'default',
    title = 'PM2.5'
  ) {

    # Get raster coordinate limits
    limits <- raster::extent(raster)
    xlim <- c(limits@xmin, limits@xmax)
    ylim <- c(limits@ymin, limits@ymax)

    layer_time <- as.numeric(stringr::str_remove(names(raster), 'X'))
    class(layer_time) <- c('POSIX', 'POSIXct')

    # Define color breaks
    if ( breaks == 'default' ) {
      breaks <- c(0,12, 35, 55, 150, 250, 350, 500)
    }

    # Get state and its counties to map
    states <- ggplot2::map_data('state', xlim = xlim, ylim = ylim)
    counties <- ggplot2::map_data('county', xlim = xlim, ylim = ylim)

    # ggplot it
    gg <- rasterVis::gplot(raster) +
      ggplot2::geom_raster(ggplot2::aes(fill = cut(.data$value, breaks))) +
      ggplot2::geom_path(data = counties,
                         ggplot2::aes( x = .data$long,
                                       y = .data$lat,
                                       group = .data$group ),
                         alpha = 0.2,
                         color = 'grey12' ) +
      ggplot2::geom_polygon( data = states,
                             fill = 'NA',
                             color = 'black',
                             ggplot2::aes( y = .data$lat,
                                           x = .data$long,
                                           group = .data$group ),
      ) +
      ggplot2::scale_fill_brewer(na.value = NA, palette = palette) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = title,
                    subtitle = layer_time,
                    x = 'Longitude', y = 'Latitude', fill = 'PM2.5') +
      ggplot2::coord_fixed(xlim = xlim, ylim = ylim, ratio = 4/3)

    return(gg)

  }
