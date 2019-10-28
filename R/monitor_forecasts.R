#' Compare Forecasts
#'
#' @param ws_monitor a ws_monitor object.
#' @param starttime an optional start time to subset with
#' @param endtime an optional end time to subset with
#' @param models a list of valid monitors to plot
#'
#' @description Overlays models of the bluesky forecasts to compare.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' pollock_pines <- monitor_load(20191007, 20191014) %>%
#'   monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")
#' monitor_forecasts(pollock_pines)
#' }
monitor_forecasts <-  function( ws_monitor,
                               starttime = NULL,
                               endtime = NULL,
                               models = list('CANSAC-1.33km', 'CANSAC-4km') ) {

  ## DEBUG
  if (FALSE) {
    models <- list('CANSAC-1.33km', 'CANSAC-4km', 'PNW-4km')
    ws_monitor <-
      monitor_load(20191007, 20191014) %>%
      monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")
  }

  ## Checks
  if ( !monitor_isMonitor(ws_monitor) ) {
    stop('Parameter ws_monitor is not correct class')
  }
  if ( monitor_isEmpty(ws_monitor) ) {
    stop('Parameter ws_monitor is empty')
  }

  # NOTE: In order to automatically select the models to use, we will need a new
  #       varible of bounding box in the netcdfs.

  # Extract target coordinates
  lon <- ws_monitor$meta$longitude
  lat <- ws_monitor$meta$latitude

  # Create bbox for bs_grid
  lon_lim <- c(lon - 0.5, lon + 0.5)
  lat_lim <- c(lat - 0.5, lat + 0.5)

  # Monitor enddate
  model_start <- as.numeric(strftime(tail(ws_monitor$data$datetime, 1), '%Y%m%d')) + 1

  # Vectorize!
  vec_bs_load <- Vectorize(bluesky_load, vectorize.args = 'model', SIMPLIFY = FALSE, USE.NAMES = TRUE)

  result <- try({
    bs_models <- vec_bs_load(
      model = models,
      modelRun = model_start,
      subDir = "forecast",
      xlim = lon_lim,
      ylim = lat_lim
    )
  }, silent = TRUE)

  if ( 'try-error' %in% class(result) ) {
    warning('An Error has occured downloading the requested models.')
  }

  # Create long time axis
  time_axis <- c(ws_monitor$data$datetime, bs_models[[1]]$time)

  # Create plot to add to
  plot( time_axis,
        seq_along(time_axis),
        las = 1,
        col = "transparent",
        ylim = c(0,700),
        xlab = 'Time',
        ylab = 'PM2.5' )

  # Add ws_monitor to plot
  monitorPlot_timeseries(ws_monitor = ws_monitor, add = T, type = 'b', lwd = 1, cex = 0.8)
  # Create colors and legend names
  cols <- RColorBrewer::brewer.pal(length(bs_models), 'Set1')
  model_name <- list()

  for ( i in 1:length(bs_models) ) {
    grid_createMonitor( bs_grid = bs_models[[i]],
                        longitude = lon,
                        latitude = lat,
                        radius = 10000,
                        monitorID = 'Model Data',
                        FUN = quantile,
                        probs = 0.8,
                        na.rm = TRUE ) %>%
      monitorPlot_timeseries(add = T, type = 'b', lwd = 1, cex = 0.8, col = cols[[i]])
    model_name[[i]] <- bs_models[[i]][['model']]
  }

  legend(
    "topleft",
    legend = model_name,#names(bs_models),
    lwd = 2,
    col = cols
  )

}
