#' @title Spaghetti Plot
#'
#' @param raster A Raster* object
#' @param longitude A target latitude
#' @param latitude A target longitude
#' @param radius A radial distance about the target location
#'
#' @description Plot a spaghetti plot of all model adjacent cells to a target
#' location
#'
#' @return a gg object
#' @export
bluesky_spaghetti <- function( raster,
                               longitude = NULL,
                               latitude = NULL,
                               radius = 5000 ) {

  # NOTE: Look into including cell counts as well as radius in the future.

  # Subset the raster to radius
  subbed <- raster_subset(raster, longitude = longitude, latitude = latitude, radius = radius)

  raster::crs(subbed) <- raster::crs(raster)

  # Use each coordinate cell -> convert to monitor -> combine mons -> plot
  coords <- raster::coordinates(subbed)


  # Generate unique names for Monitor Identification
  unique_names <- mapply(
    function(x,y) {
      paste('monitor', x, y, sep = '_')
    },
    coords[,1],
    coords[,2]
  )

  # Convert each raster to monitor at each cell coordinate
  monitor_list <- mapply(
    function(x,y,n) {
      raster_toMonitor(raster, x, y, monitorID = n)
    },
    coords[,1],
    coords[,2],
    unique_names,
    SIMPLIFY = FALSE
  )

  # Combine all the monitors
  monitors <- PWFSLSmoke::monitor_combine(monitor_list)

  # Plot the monitors`
  gg <- ggplot2::ggplot( data = PWFSLSmoke::monitor_toTidy(monitors),
                         ggplot2::aes_(x = ~datetime, y = ~pm25) ) +
    ggplot2::geom_line(ggplot2::aes_(color = ~monitorID)) +
    ggplot2::labs( x = 'Datetime',
                   y = '\u03bcg / m\u00b3',
                   title = expression('PM'[2.5])) +
    ggplot2::guides(color = FALSE)


  return(gg)

}

