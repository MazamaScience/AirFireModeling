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
raster_spaghetti <- function( raster,
                               longitude = NULL,
                               latitude = NULL,
                               radius = 5000 ) {

  # Checks
  if ( !grepl('Raster', class(raster)[1]) ) {
    stop(print('A valid Raster object is required.'))
  }
  if ( longitude < raster::xmin(raster) | longitude > raster::xmax(raster) |
       latitude < raster::ymin(raster) | latitude > raster::ymax(raster) ) {
    stop('Check Coordinates: Out of range.')
  }



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


  # Create Target Distance field for line alpha
  monitors$meta$target_dist <- geosphere::distGeo(c(longitude, latitude), coords)

  # Tidyify
  df <- PWFSLSmoke::monitor_toTidy(monitors)

  # Plot the monitors
  gg <- ggplot2::ggplot( data = df,
                         ggplot2::aes_(x = ~datetime, y = ~pm25) ) +
    # Organize by the distance. Less distance = Greater Alpha
    ggplot2::geom_line(ggplot2::aes_(group = ~monitorID, alpha = ~-target_dist), color = 'dodgerblue4') +
    ggplot2::labs( x = 'Datetime',
                   y = '\u03bcg / m\u00b3',
                   title = expression('PM'[2.5])) +
    ggplot2::guides(alpha = FALSE)


  return(gg)

}

