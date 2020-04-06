#' @export
#' @importFrom stats quantile
#' @importFrom utils tail
#' @importFrom graphics plot legend
#'
#' @title Compare forecasts
#'
#' @description Plots monitor data and overlays models of the bluesky forecasts.
#'
#' For each model, a timeseries is created by gathering \code{count} grid cells
#' within \code{radius} meters of the \code{ws_monitor}. The resulting set of
#' timeseries are then collapsed into a single timeseries by applying the
#' \code{quantile()} using the user specified probability \code{prob}.
#'
#' @param ws_monitor ws_monitor object.
#' @param model Model identifier(s).
#' @param modelType Subdirectory path containing BlueSky output, i.e. 'forcast'.
#' @param baseUrl Base URL for BlueSky output.
#' @param radius Distance (km) of radius from target location.
#' @param count Number of grid cells within radius to return.
#' @param prob Quantile probability used when  plotting model data.
#' @param verbose Logical to display messages.
#' @param ... Additional arguments passed to
#' \code{PWFSLSmoke::monitor_timeseriesPlot()}.
#'
#' @examples
#' \dontrun{
#' library(PWFSLSmoke)
#' initializeMazamaSpatialUtils()
#'
#' library(AirFireModeling)
#' setModelDataDir('~/Data/Bluesky')
#'
#' San_Pablo <-
#'   monitor_load(20191025, 20191029) %>%
#'   monitor_subset(monitorIDs = '060131004_01')
#'
#' models <- bluesky_findModels(
#'   yosemite_village$meta$longitude,
#'   yosemite_village$meta$latitude
#' )
#'
#' monitor_forecastPlot(
#'   San_Pabo,
#'   model = models
#' )
#'
#' }
monitor_forecastPlot <-  function(
  ws_monitor,
  model = NULL,
  modelType = "forecast",
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  radius = 20,
  count = 9,
  prob = 0.5,
  verbose = TRUE,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(ws_monitor)

  if ( !PWFSLSmoke::monitor_isMonitor(ws_monitor) )
    stop("Parameter ws_monitor is not not a valid ws_monitor object")

  if ( PWFSLSmoke::monitor_isEmpty(ws_monitor) )
    stop("Parameter ws_monitor is empty")

  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be numeric.")

  if ( !is.null(count) ) {
    if ( !is.numeric(count) )
      stop("Parameter 'count' must be numeric.")
  }

  longitude <- ws_monitor$meta$longitude
  latitude <- ws_monitor$meta$latitude

  availableModels <- bluesky_findModels(longitude, latitude)
  modelIsValid <- model %in% availableModels
  if ( any(!modelIsValid) ) {
    missingModels <- model[!modelIsValid]
    stop(paste0(
      "These models do not cover this location: ",
      paste0(missingModels, collapse = ", ")
    ))
  }

  # ----- Read in model data ---------------------------------------------------

  # Create bbox ~100 km wide and tall, larger than any reasonable radius
  xlim <- c(longitude - 0.5, longitude + 0.5)
  ylim <- c(latitude - 0.5, latitude + 0.5)

  # Monitor enddate
  monitor_endtime <- utils::tail(ws_monitor$data$datetime, 1)

  # TODO:  Need better logic to get the latest completed run for each model

  # NOTE:  Both monitor data and model runs specify time in UTC
  model_starttime <- lubridate::floor_date(monitor_endtime, unit = "day")
  modelRun <- strftime(model_starttime, "%Y%m%d00", tz = "UTC")

  # Create a list of "fake" monitors derived from model data

  fakeMonitorList <- list()
  for ( modelName in model ) {

    result <- try({

      fakeMonitorList[[modelName]] <-
        # Load each model
        bluesky_load(
          model = modelName,
          modelRun = modelRun,
          modelType = modelType,
          xlim = xlim,
          ylim = ylim
        ) %>%
        # Subset and convert to ws_monitor
        raster_toMonitor(
          longitude = longitude,
          latitude = latitude,
          radius = radius,
          count = count,
          rasterName = modelName
        ) %>%
        # Collapse to a single ws_monitor
        PWFSLSmoke::monitor_collapse(
          monitorID = modelName,
          FUN = quantile,
          probs = prob,
          na.rm = TRUE
        )

    }, silent = TRUE)

    if ( "try-error" %in% result ) {
      warning(sprintf("Unable to load %s %s for %s", model, modelType, modelRun))
    }

  }

  # NOTE:  Combining monitors ensures that they share a common time axis.

  # Combine the monitors
  fakeMonitors <- PWFSLSmoke::monitor_combine(fakeMonitorList)
  allMonitors <- PWFSLSmoke::monitor_combine(list(ws_monitor, fakeMonitors))

  # ----- Create plot --------------------------------------------------------

  # Plot it once with transparent colors to get the right y scaling
  argsList <- list(...)
  argsList$ws_monitor <- allMonitors
  argsList$col <- 'transparent'
  do.call(PWFSLSmoke::monitor_timeseriesPlot, argsList)

  # Now add the ws_monitor data
  argsList <- list(...)
  argsList$ws_monitor <- allMonitors
  argsList$monitorID <- ws_monitor$meta$monitorID
  do.call(PWFSLSmoke::monitor_timeseriesPlot, argsList)

  # Create a list of more colors than we will use
  cols <- RColorBrewer::brewer.pal(9, 'Set1')

  # Add all the "fake" monitors
  i <- 0
  for ( name in names(fakeMonitorList) ) {

    i <- i + 1
    PWFSLSmoke::monitor_timeseriesPlot(
      fakeMonitorList[[name]],
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

  graphics::title(ws_monitor$meta$siteName)

}


# ===== DEBUGGING ==============================================================

if (FALSE) {

  library(PWFSLSmoke)
  initializeMazamaSpatialUtils()

  library(AirFireModeling)
  setModelDataDir('~/Data/Bluesky')

  ws_monitor <-
    PWFSLSmoke::monitor_load(20191007, 20191014) %>%
    PWFSLSmoke::monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")

  ws_monitor
  model = c('CANSAC-1.33km', 'CANSAC-4km')
  modelType = "forecast"
  radius = 20
  count = 9
  prob = 0.5
  verbose = TRUE

  monitor_forecastPlot(
    ws_monitor,
    model = c('CANSAC-1.33km', 'CANSAC-4km'),
    modelType = "forecast",
    radius = 20,
    count = 9,
    prob = 0.5,
    verbose = TRUE
  )

  # Napa
  ws_monitor <-
    PWFSLSmoke::monitor_load(20191015, 20191026) %>%
    PWFSLSmoke::monitor_subset(monitorIDs = "060131004_01")

  model <- c('CANSAC-1.33km', 'CANSAC-4km')

  monitor_forecastPlot(ws_monitor, model = model)

}

