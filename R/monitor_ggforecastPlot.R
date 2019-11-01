#' @export
#' @importFrom stats quantile
#' @importFrom utils tail
#' @importFrom graphics plot legend
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
#'
#' @examples
#' \dontrun{
#' library(PWFSLSmoke)
#' initializeMazamaSpatialUtils()
#'
#' library(AirFireModeling)
#' setModelDataDir('~/Data/Bluesky')
#'
#' yosemite_village <-
#'   monitor_load(20191025, 20191029) %>%
#'   monitor_subset(monitorIDs = '060131004_01')
#'
#' monitor_ggforecastPlot(yosemite_village)
#' }
monitor_ggforecastPlot <- function(
  ws_monitor,
  starttime = NULL,
  endtime = NULL,
  models = c('CANSAC-1.33km', 'CANSAC-4km') ,
  subDir = 'forecast'
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

  # Combine the monitors
  fakeMonitors <- PWFSLSmoke::monitor_combine(fakeMonitorList)
  allMonitors <- PWFSLSmoke::monitor_combine(list(ws_monitor, fakeMonitors))

  # ----- ggplot ---------------------------------------------------------------

  gg <- AirMonitorPlots::ggplot_pm25Timeseries(allMonitors) +
    AirMonitorPlots::geom_pm25Points(ggplot2::aes(color = .data$monitorID)) +
    AirMonitorPlots::stat_nowcast(ggplot2::aes(color = .data$monitorID))

  return(gg)

}
