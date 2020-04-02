#' @export
#' @title Spaghetti Plot
#'
#' @param raster A Raster\* object or a list of Raster\* objects.
#' @param longitude Target longitude from which the radius will be calculated.
#' @param latitude Target latitude from which the radius will be calculated.
#' @param radius Distance (km) of radius from target location.
#' @param count Number of grid cells within radius to return.
#' @param monitorID PWFSLSmoke monitorID used to retrieve and plot monitor data.
#'
#' @description Plot a spaghetti plot of all model adjacent cells to a target
#' location
#'
#' @seealso \code{\link{raster_subsetByDistance}}
#'
#' @return A ggplot object.
#'
#' @examples
#'
raster_spaghettiPlot <- function(
  raster = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = 50,
  count = NULL,
  monitorID = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)

  if ( class(raster) != "list" &&
       !stringr::str_detect(class(raster), 'Raster*') )
    stop("Parameter 'raster' must be a single or a list of Raster* objects.")

  if ( !is.numeric(longitude) )
    stop("Parameter 'longitude' must be numeric.")

  if ( !is.numeric(latitude) )
    stop("Parameter 'latitude' must be numeric.")

  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be numeric.")

  if ( !is.null(count) ) {
    if ( !is.numeric(count) )
      stop("Parameter 'count' must be numeric.")
  }

  # Check domain
  if ( class(raster) == 'list' ) {
    r <- raster[[1]]
  } else {
    r <- raster
  }

  if ( longitude < raster::xmin(r) | longitude > raster::xmax(r) |
       latitude < raster::ymin(r)  |  latitude > raster::ymax(r) ) {
    stop('Check Coordinates: Target location is outside the raster domain.')
  }

  # ----- Prepare data ---------------------------------------------------------

  localRaster <- raster_subsetByDistance(
    raster,
    longitude = longitude,
    latitude = latitude,
    radius = radius,
    count = count
  )




  # # # NOTE: Look into including cell counts as well as radius in the future.
  # # args <- list(...)
  # # if ( 'radius' %in% names(args) ) {
  # #   subbed <- raster_subsetByDistance( raster,
  # #                                      longitude = longitude,
  # #                                      latitude = latitude,
  # #                                      radius = args[['radius']] )
  # # } else if ('n' %in% names(args) ) {
  # #   subbed <- raster_subsetByDistance( raster,
  # #                                      longitude = longitude,
  # #                                      latitude = latitude,
  # #                                      n = args[['n']],
  # #                                      snapToGrid = args[['snapToGrid']] )
  # # } else {
  # #   stop('Must provide subset parameter')
  # # }
  #
  # # NOTE: Look into including cell counts as well as radius in the future.
  # args <- list(...)
  # if ( 'radius' %in% names(args) ) {
  #   subbed <- raster_subsetByDistance( raster,
  #                                      longitude = longitude,
  #                                      latitude = latitude,
  #                                      radius = args[['radius']] )
  # } else if ('n' %in% names(args) ) {
  #   subbed <- raster_subsetByDistance( raster,
  #                                      longitude = longitude,
  #                                      latitude = latitude,
  #                                      radius = args[['radius']],
  #                                      count = args[['n']] )
  # } else {
  #   stop('Must provide subset parameter')
  # }

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

