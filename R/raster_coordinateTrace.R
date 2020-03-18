raster_coordinateTrace <- function( raster,
                                    longitude = NULL,
                                    latitude = NULL,
                                    monitorID = NULL,
                                    model = c('PNW-4km', 'PNW-1.33km'),
                                    ... ) {
  .load_monitor <- function(r) {
  # Check if lon lat is in coordinate domain
  if ( abs(longitude) > abs(r@extent@xmax) ||
       abs(longitude) < abs(r@extent@xmin) ) {
    stop('Longitude not within domain.')
  }
  if ( abs(latitude) > abs(r@extent@ymax) ||
       abs(latitude) < abs(r@extent@ymin) ) {
    stop('Latitude not within domain.')
  }
  # Parse dates stored in model
  model_dates <- as.numeric(stringr::str_remove(r@data@names, pattern = 'X')) # model dates stored in layer name
  class(model_dates) <- c('POSIXct', 'POSIXt')
  attr(model_dates, 'tzone') <- 'UTC'
  # Load All monitors for model dates
  startdate <- range(model_dates)[1]
  enddate <- range(model_dates)[2]
  monitors <- PWFSLSmoke::monitor_load(startdate, enddate)

  # If Monitor ID is not provided, load nearest one unless specified otherwise
  if ( is.null(monitorID) ) {
    monitors_dist <- geosphere::distHaversine( cbind(monitors$meta$longitude,
                                                     monitors$meta$latitude),
                                               cbind(longitude, latitude) )
    nearest_monitorID <- monitors$meta$monitorID[which.min(monitors_dist)]
    target_dist <- monitors_dist[which.min(monitors_dist)]
    target_monitor <- PWFSLSmoke::monitor_subset(monitors, monitorIDs = nearest_monitorID)
  } else {
    target_monitor <- PWFSLSmoke::monitor_subset(monitors, monitorIDs = monitorID)
  }
  # Overwrite coordinates unless specified
  longitude <- ifelse(is.null(longitude), target_monitor$meta$longitude, longitude)
  latitude <- ifelse(is.null(latitude), target_monitor$meta$latitude, latitude)
  model_name <- stringr::str_extract(r@file@name, '(?<=[/])([^/]+)(?=\\_\\d+.[^.]+)')
  model_monitor <- raster_toMonitor(raster, longitude, latitude, monitorID = model_name)
  return(model_monitor)
}
  combined <- PWFSLSmoke::monitor_combine(list(model_monitor, target_monitor))



  gg <- AirMonitorPlots::ggplot_pm25Timeseries(combined) +
    AirMonitorPlots::geom_pm25Points(ggplot2::aes(color = .data$monitorID)) +
    AirMonitorPlots::stat_nowcast(ggplot2::aes(color = .data$monitorID)) +
    ggplot2::labs(color = 'Legend') +
    ggplot2::theme(legend.position = 'bottom', legend.margin = ggplot2::margin(), legend.box = 'vertical')
  return(gg)
}
