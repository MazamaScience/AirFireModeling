#' @export
#' @importFrom utils tail
#' @importFrom PWFSLSmoke monitor_isMonitor monitor_isEmpty
#' @importFrom rlang .data
#'
#' @title Compare forecasts
#'
#' @description Overlays models of the bluesky forecasts to compare with ggplot.
#'
#' @param ws_monitor a ws_monitor object.
#' @param starttime an optional start time to subset with
#' @param endtime an optional end time to subset with
#' @param models a list of valid monitors to plot
#' @param subDir Subdirectory path containing netcdf data. (Passed to
#' \code{bluesky_load()})
#' @param buffer a radial buffer around the selected monitor to collapse the
#' model about.
monitor_forecast <- function(
  ws_monitor,
  starttime = NULL,
  endtime = NULL,
  models = c('CANSAC-1.33km', 'CANSAC-4km'),
  subDir = 'forecast',
  buffer = 2000
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(ws_monitor)

  if ( !PWFSLSmoke::monitor_isMonitor(ws_monitor) )
    stop("Parameter ws_monitor is not not a valid ws_monitor object")

  if ( PWFSLSmoke::monitor_isEmpty(ws_monitor) )
    stop("Parameter ws_monitor is empty")


  # ----- Read in model data ---------------------------------------------------

  # Extract target coordinates
  lon <- ws_monitor$meta$longitude
  lat <- ws_monitor$meta$latitude

  # TODO: Auto gather avaliable bluesky models
  in_model <- bluesky_availiableModels(longitude = lon, latitude = lat)

  # Monitor enddate
  monitor_endtime <- utils::tail(ws_monitor$data$datetime, 1)

  # TODO:  Need better logic to get the latest completed run for each model

  # NOTE:  Both monitor data and model runs specify time in UTC
  model_starttime <- lubridate::floor_date(monitor_endtime, unit = "day")
  modelRun <- strftime(model_starttime, "%Y%m%d00", tz = "UTC")

  # Load the bluesky rasters
  # Create thread cluster
  cl <- parallel::makeCluster(future::availableCores() - 1, timeout = 60)
  future::plan(strategy = future::cluster, workers = cl)

  bs_monitorList <- list()
  for ( model in models ) {
    bs_monitorList[[model]] <- future::future({
      setModelDataDir('~/Data/Bluesky')
      bs_raster <- bluesky_load( modelRun = modelRun,
                                 subDir = subDir,
                                 model = model )
      raster_toMonitor( raster = bs_raster,
                        longitude = lon,
                        latitude = lat,
                        buffer = buffer,
                        FUN = mean, # Mean of the buffer region
                        monitorID = model )
    })
  }
  # print loading bar
  cat("Loading ")
  while(!future::resolved(bs_monitorList[[1]])) {
    cat("=")
    Sys.sleep(2)
  }
  cat("\n")

  # Combine the bs_monitors and ws_monitor
  monitors <- PWFSLSmoke::monitor_combine(
    list( ws_monitor, PWFSLSmoke::monitor_combine(future::values(bs_monitorList)) )
  )
  future::autoStopCluster(cl)
  # ----- ggplot ---------------------------------------------------------------

  gg <- AirMonitorPlots::ggplot_pm25Timeseries(monitors) +
    AirMonitorPlots::geom_pm25Points(ggplot2::aes(color = .data$monitorID)) +
    AirMonitorPlots::stat_nowcast(ggplot2::aes(color = .data$monitorID)) +
    ggplot2::labs(color = 'Legend')

  print(gg)

  # Return combined monitors
  return(monitors)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(PWFSLSmoke)
  
  ws_monitor <- monitor_load(20191115, 20191118, monitorIDs = '060670010_01')
  starttime = NULL
  endtime = NULL
  models = c('CANSAC-1.33km', 'CANSAC-4km')
  subDir = 'forecast'
  buffer = 2000
  version = "3.5"
  
  setModelDataDir('~/Data/Bluesky')
  monitor_forecast(ws_monitor)
  
}

