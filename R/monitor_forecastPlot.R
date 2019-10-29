#' @export
#' @importFrom stats quantile
#' @importFrom utils tail
#' @importFrom graphics plot legend
#'
#' @title Compare forecasts
#'
#' @description Overlays models of the bluesky forecasts to compare.
#'
#' @param ws_monitor a ws_monitor object.
#' @param starttime an optional start time to subset with
#' @param endtime an optional end time to subset with
#' @param models a list of valid monitors to plot
#' @param subDir Subdirectory path containing netcdf data. (Passed to 
#' \code{bluesky_load()})
#'
#' @examples
#' \dontrun{
#' library(PWFSLSmoke)
#' initializeMazamaSpatialUtils()
#' 
#' library(AirFireModeling)
#' setModelDataDir('~/Data/Bluesky')
#' 
#' pollock_pines <- 
#'   monitor_load(20191007, 20191014) %>%
#'   monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")
#'   
#' monitor_forecastPlot(pollock_pines)
#' }
monitor_forecastPlot <-  function(
  ws_monitor,
  starttime = NULL,
  endtime = NULL,
  models = c('CANSAC-1.33km', 'CANSAC-4km') ,
  subDir = "combined"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(ws_monitor)
  
  if ( !PWFSLSmoke::monitor_isMonitor(ws_monitor) )
    stop("Parameter ws_monitor is not not a valid ws_monitor object")
  
  if ( PWFSLSmoke::monitor_isEmpty(ws_monitor) )
    stop("Parameter ws_monitor is empty")
  
  
  # ----- Read in model data ---------------------------------------------------
  
  # TODO:  Use a package internal list of model domains to automatically select 
  # TODO:  which models can be used.
  
  # Extract target coordinates
  lon <- ws_monitor$meta$longitude
  lat <- ws_monitor$meta$latitude
  
  # Create bbox for bs_grid ~100 km wide and tall, larger than the radius
  xlim <- c(lon - 0.5, lon + 0.5)
  ylim <- c(lat - 0.5, lat + 0.5)
  
  # Monitor enddate
  monitor_endtime <- utils::tail(ws_monitor$data$datetime, 1)
  
  # TODO:  Need better logic to get the latest completed run for each model
  
  # NOTE:  Both monitor data and model runs specify time in UTC
  model_starttime <- lubridate::floor_date(monitor_endtime, unit = "day")
  modelRun <- strftime(model_starttime, "%Y%m%d00", tz = "UTC")
  
  fakeMonitorList <- list()
  for ( model in models ) {
    
    result <- try({
      fakeMonitorList[[model]] <-
        bluesky_load(
          model = model, 
          modelRun = modelRun, 
          subDir = subDir,
          xlim = xlim, 
          ylim = ylim
        ) %>%
        grid_createMonitor(
          longitude = lon,
          latitude = lat,
          radius = 10000,
          monitorID = model,
          FUN = quantile,
          probs = 0.5,
          na.rm = TRUE
        )
      
    }, silent = TRUE)
    
    if ( "try-error" %in% result ) {
      warning(sprintf("Unable to load %s %s for %s", model, subDir, modelRun))
    }
    
  }
  
  # ----- Create ggplot --------------------------------------------------------
  
  # TODO:  An alternative solution is to combine all the monitors and then use
  # TODO:  ggplot2 graphics from AirMonitorPlots to plot them all at once like
  # TODO:  this:
  
  fakeMonitors <- PWFSLSmoke::monitor_combine(fakeMonitorList)
  allMonitors <- PWFSLSmoke::monitor_combine(list(ws_monitor, fakeMonitors))
  
  # AirMonitorPlots code goes here
  
  # ----- Create plot ----------------------------------------------------------
  
  # TODO:  Combine all time axes before sort(unique(...))
  
  # Create long time axis
  time_axis <- sort(unique(c(ws_monitor$data$datetime, fakeMonitorList[[1]]$data$datetime)))

  xlim_plot <- range(time_axis)
  ylim_plot <- c(0, max(allMonitors$data[,-1], na.rm = TRUE))
  
  # TODO:  Allow most of these parameters to be specified as arguments or as
  # TODO:  part of ...
  
  # Add ws_monitor to plot
  PWFSLSmoke::monitorPlot_timeseries(
    ws_monitor = ws_monitor, 
    xlim = xlim_plot,
    ylim = ylim_plot,
    type = 'b', 
    lwd = 1, 
    cex = 0.8
  )
  
  # Create colors and legend names
  cols <- RColorBrewer::brewer.pal(length(fakeMonitorList), 'Set1')

  i <- 0
  for ( model in names(fakeMonitorList) ) {
    
    i <- i + 1
    PWFSLSmoke::monitor_timeseriesPlot(
      fakeMonitorList[[model]],
      add = TRUE,
      type = "b",
      lwd = 1,
      cex = 0.8,
      col = cols[[i]]
    )
    
  }
  
  legend(
    "topleft",
    legend = names(fakeMonitorList),
    lwd = 2,
    col = cols
  )
  
}

# ===== DEBUGGING ============================================================

if (FALSE) {

  library(PWFSLSmoke)
  initializeMazamaSpatialUtils()

  library(AirFireModeling)
  setModelDataDir('~/Data/Bluesky')
  
  ws_monitor <-
    PWFSLSmoke::monitor_load(20191007, 20191014) %>%
    PWFSLSmoke::monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")
  starttime <- NULL
  endtime <- NULL
  models <- list('CANSAC-1.33km', 'CANSAC-4km', 'PNW-4km')
  
  monitor_forecastPlot(ws_monitor, models = models)
  
  # Napa
  ws_monitor <-
    PWFSLSmoke::monitor_loadLatest() %>%
    PWFSLSmoke::monitor_subset(monitorIDs = "060131004_01")
  models <- list('CANSAC-1.33km', 'CANSAC-4km')

  monitor_forecastPlot(ws_monitor, models = models)
  
}

