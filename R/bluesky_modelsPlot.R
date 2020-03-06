#' Title
#'
#' @param modelRun Date code as "YYYYMMDDHH".
#' @param latitude A target latitude
#' @param longitude A target longitude
#' @param monitorID (Optional) a monitorID to plot along same model run period.
#' @param models A list of models to plot
#' @param ... additional parameters passed into \code{bluesky_load()}
#'
#' @return
#' @export
#'
#' @examples
bluesky_modelsPlot <- function( modelRun,
                               latitude = NULL,
                               longitude = NULL,
                               monitorID = NULL,
                               models = c('PNW-1.33km', 'PNW-4km'),
                               ... ) {

  # load BlueSky models
  bs_list <- bluesky_load(modelRun = modelRun, model = models, ...)

  # Create clusters
  cl <- parallel::makeCluster(future::availableCores() - 1, timeout = 60)
  future::plan(strategy = future::cluster, workers = cl)

  # If a monitor ID is provided, download the monitor for plot
  monitor_list <- list()
  if ( !is.null(monitorID) ) {
    # Assume names of raster layers are POSIX dates
    # Remove 'X' from string convert to numeric
    datetime <- as.numeric(stringr::str_remove( string = names(bs_list[[1]]),
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
    monitor_list[[monitorID]] <- future::future({
      PWFSLSmoke::monitor_load( startdate = sd,
                                enddate = ed,
                                monitorIDs = monitorID )
      })
  }

  # Convert bluesky rasters to monitors via coordinates
  for ( i in names(bs_list) ) {
    monitor_list[[i]] <- future::future({
      raster_toMonitor( raster = bs_list[[i]],
                        longitude = longitude,
                        latitude = latitude,
                        monitorID = i,
                        FUN = mean )
    })
  }

  # combine monitor list
  monitors <- PWFSLSmoke::monitor_combine(future::values(monitor_list))

  # close thread connections
  future::autoStopCluster(cl)

  # Plot it
  gg <- AirMonitorPlots::ggplot_pm25Timeseries(PWFSLSmoke::monitor_toTidy(monitors)) +
    AirMonitorPlots::geom_pm25Points(ggplot2::aes(color = .data$monitorID)) +
    AirMonitorPlots::stat_nowcast(ggplot2::aes(color = .data$monitorID)) +

  return(gg)

}
