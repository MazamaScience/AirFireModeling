#' @title Spaghetti Plot
#'
#' @param raster A Raster* object
#' @param longitude A target latitude
#' @param latitude A target longitude
#' @param ... See details.
#'
#' @description Plot a spaghetti plot of all model adjacent cells to a target
#' location
#'
#' @details \code{radius}: radial (meters) subset by distance from target location.
#' \code{n}: integer subset by the targets adjacent cell count.
#'
#' @return a gg object
#' @export
raster_spaghetti <- function( raster,
                              longitude = NULL,
                              latitude = NULL,
                              ... ) {

  # Checks
  if ( !grepl('[rR]aster.+', class(raster)) ) {
    stop(print('A valid Raster object is required.'))
  }
  if ( longitude < raster::xmin(raster) | longitude > raster::xmax(raster) |
       latitude < raster::ymin(raster)  |  latitude > raster::ymax(raster) ) {
    stop('Check Coordinates: Out of range.')
  }

  # NOTE: Look into including cell counts as well as radius in the future.
  args <- list(...)
  if ( 'radius' %in% names(args) ) {
    subbed <- raster_subset( raster,
                             longitude = longitude,
                             latitude = latitude,
                             radius = args[['radius']] )
  } else if ('n' %in% names(args) ) {
    subbed <- raster_subset( raster,
                             longitude = longitude,
                             latitude = latitude,
                             n = args[['n']],
                             snapToGrid = args[['snapToGrid']] )
  } else {
    stop('Must provide subset parameter')
  }

  # color param
  if ( 'color' %in% names(args) ) {
    color <- args[['color']]
  } else {
    color <- 'dodgerblue4'
  }

  # Alpha Param
  if ( 'alpha' %in% names(args) ) {
    alpha <- args[['alpha']]
  } else {
    alpha <- ~-target_dist
  }

  if ( 'title' %in% names(args) ) {
    title <- args[['title']]
  } else {
    title <- expression('PM'[2.5])
  }



  # If monitor ID is provided, load it and plot it
  if ( 'monitorID' %in% names(args) & !is.null(args[['monitorID']]) ) {
    # Assume names of raster layers are POSIX dates
    # Remove 'X' from string convert to numeric
    datetime <- as.numeric(stringr::str_remove( string = names(raster),
                                                pattern = 'X' ))
    # TIMEZONES!
    tzone <- c('UTC')
    # Assimilate datetime class
    class(datetime) <- c('POSIXct', 'POSIXt')
    # Set TIMEZONE to UTC
    attr(datetime, 'tzone') <- tzone
    sd <- range(datetime)[1]
    ed <- range(datetime)[2]
    # Load monitor
    monitor <- PWFSLSmoke::monitor_load(startdate = sd, enddate = ed, monitorIDs = args[['monitorID']])
    df <- monitor$data
    names(df) <- c('datetime', 'pm25')
    # Plot
    gg_monitor <- ggplot2::geom_line(ggplot2::aes_(x = ~datetime, y = ~pm25), data = df, linetype = 'dashed')
  }

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
    function(x,y,z) {
      raster_toMonitor(raster, x, y, monitorID = z)
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
    ggplot2::geom_line( ggplot2::aes_(group = ~monitorID, alpha = alpha),
                        color = color ) +
    ggplot2::labs( x = 'Datetime',
                   y = '\u03bcg / m\u00b3',
                   title = title ) +
    ggplot2::guides(alpha = FALSE)

  if ( exists('gg_monitor') ) {
    return(gg + gg_monitor)
  } else {
    return(gg)
  }
}

