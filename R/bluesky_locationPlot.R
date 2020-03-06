#' Downlaod and Plot BlueSky location models
#'
#' @param modelRun Date code as "YYYYMMDDHH".
#' @param latitude A target latitude
#' @param longitude A target longitude
#' @param monitorID (Optional) a monitorID to plot along same model run period.
#' @param models A list of models to plot
#' @param ... additional parameters passed into \code{bluesky_load()}
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun {
#' bluesky_multiplot(20191212, latitude, longitude, version = "4.1", monitorID = '530770016_01')
#' }
bluesky_locationPlot <- function( modelRun,
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
    monitor_list[[monitorID]] <- PWFSLSmoke::monitor_load( startdate = sd,
                                                           enddate = ed,
                                                           monitorIDs = monitorID )

    if ( is.null(latitude) | is.null(longitude) ) {
      latitude <- monitor_list[[monitorID]]$meta$latitude
      longitude <- monitor_list[[monitorID]]$meta$longitude
      message(paste0('Using Monitor Location ', latitude, ' ', longitude))
    } else {
      message('Overwriting Monitor Location with params: longitude, latitude')
    }
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

  df <- PWFSLSmoke::monitor_toTidy(monitors)
  # Plot it
  gg <- AirMonitorPlots::ggplot_pm25Timeseries(ws_data = df) +
    AirMonitorPlots::geom_pm25Points(ggplot2::aes(color = .data$monitorID)) +
    AirMonitorPlots::stat_nowcast(ggplot2::aes(color = .data$monitorID)) +
    ggplot2::labs(color = 'Legend')

  return(gg)

  if ( FALSE ) {
    monitorID = '410650007_01'
    bluesky_locationPlot(20200301, monitorID = monitorID, models = c('PNW-1.33km', 'PNW-4km', 'CANSAC-4km'))
  }

}
