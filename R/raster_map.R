#' @title Plot Model Maps
#'
#' @description Plot a mapped region of a Raster object. Automatically downloads
#' region shape files.
#'
#' @param raster A Raster\* object or a list of Raster\* objects.
#' @param index The index of the Raster* object. See details.
#' @param palette The color palette used to map cell values.
#' @param breaks The breaks used to define the map cell color.
#' @param title (Optional) A plot title.
#' @param direction Numeric. \code{direction = -1} reverses color palette.
#' @param timezone The timezone to parse the dates. Default is UTC.
#' @param verbose Show messages.
#'
#' @details If a list of Raster\* objects is provided, a small-multiples plot will be returned.
#' The \code{index} is typically associated with a time-axis or RasterLayer,
#' e.g \code{index = 1} is the first hour of a model.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/BlueSky/')
#' bs <- raster_load()
#' raster_map(bs)
#' }
raster_map <- function( raster,
                        index = 1,
                        palette = 'Greys',
                        breaks = 'default',
                        title = 'PM2.5',
                        direction = 1,
                        timezone = 'UTC',
                        verbose = TRUE ) {
  UseMethod('raster_map', raster)
}

#' @describeIn raster_map A Raster object implementation.
#' @export
raster_map.Raster <-function( raster,
                              index = 1,
                              palette = 'Greys',
                              breaks = 'default',
                              title = 'PM2.5',
                              direction = 1,
                              timezone = 'UTC',
                              verbose = TRUE ) {

  if ( verbose ) message('A raster')
  if ( index > dim(raster)[3] ) {
    stop('Index out of range.')
  }
  layer <- raster[[index]]
  .plot_map(layer, title, timezone, breaks, direction, palette)

}

#' @describeIn raster_map A multithreaded implementation for iterating over lists.
#' @export
raster_map.list <- function( raster,
                             index = 1,
                             palette = 'Greys',
                             breaks = 'default',
                             title = 'PM2.5',
                             direction = 1,
                             timezone = 'UTC',
                             verbose = TRUE ) {

  if ( verbose ) {
    message(paste0('Plotting ', length(raster), ' models ...'))
  }

  layer_list <- lapply(
    X = raster,
    FUN = function(r) {
      if ( index > dim(r)[3] ) {
        stop('Index out of range.')
      }
      return(r[[index]])
    }
  )

  cl <- parallel::makeCluster(future::availableCores() - 1)
  future::plan(strategy = future::cluster, workers = cl)
  gg_list <- future.apply::future_lapply(
    X = layer_list,
    FUN = function(r) {
      .plot_map(r, title, timezone, breaks, direction, palette)
    }
  )
  parallel::stopCluster(cl)

  return(cowplot::plot_grid(plotlist = gg_list))

}

# NOTE: Sub-internal function for plotting an individual ggplot2 raster layer
.plot_map <- function(raster, title, timezone, breaks, direction, palette) {
  # Get layer coordinate limits
  limits <- raster::extent(raster)
  xlim <- c(limits@xmin, limits@xmax)
  ylim <- c(limits@ymin, limits@ymax)

  layer_time <- as.numeric(stringr::str_remove(names(raster), 'X'))
  class(layer_time) <- c('POSIX', 'POSIXct')
  attr(layer_time, 'tzone') <- timezone
  layer_time <- as.character.Date(layer_time)

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
    ggplot2::scale_fill_brewer( na.value = NA,
                                palette = palette,
                                direction = direction ) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = title,
                  subtitle = layer_time,
                  x = 'Longitude', y = 'Latitude', fill = 'PM2.5') +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim, ratio = 4/3)

  return(gg)

}

