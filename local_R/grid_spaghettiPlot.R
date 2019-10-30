#' @keywords bs_grid, spaghettiPlot
#' @export
#' @title Create a Spaghetti Plot From ws_monitor Data
#' @param bs_grid a bs_grid list
#' @param longitude target longitude
#' @param latitude target latitude
#' @param count count of closest grid cells to be included in the plot
#' @param rmWidth rolling mean width in hours
#' @param ... additional arguments to pass to plot() via monitorPlot_timeseries()
#' @description Creates a "spaghetti plot" timeseries  with a line for each of the
#' \code{count} model grid cells closest to the target location. A centered
#' rolling mean is applied to each timeseries before plotting.
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bluesky <- bluesky_aggregate(model = 'PNW-4km', firstModelRun = 20190915, lastModelRun = 20190917)
#' gridPlot_spaghetti(bluesky, longitude=-117.400, latitude=47.660)
#' }

gridPlot_spaghetti <- function(bs_grid, longitude=NULL, latitude=NULL,
                               count=9, rmWidth=3, ...) {

  # Sanity check
  if ( is.null(longitude) | is.null(latitude) ) {
    stop("Need to specify target coordinates.")
  }

  # ----- Set up arguments list -----------------------------------------------------------

  argsList <- list(...)

  if( !('type' %in% names(argsList)) ) {
    argsList$type <- 'l'
  }

  if ( !('lwd' %in% names(argsList)) ) {
    argsList$lwd <- 3
  }

  if ( !('col' %in% names(argsList)) ) {
    argsList$col <- 'lightblue'
  }

  if ( !('ylab' %in% names(argsList)) ) {
    argsList$ylab <- "PM2.5"
  }

  if ( !('main' %in% names(argsList)) ) {
    argsList$main <- "PM2.5"
  }

  if ( !('add' %in% names(argsList)) ) {
    argsList$add <- FALSE
  }

  ws_monitors <- grid_subsetByDistance(bs_grid, longitude=longitude, latitude=latitude, count=count)
  rollingMeans <- monitor_rollingMean(ws_monitors, rmWidth)
  argsList$ws_monitor <- rollingMeans

  # ----- Create plot -----------------------------------------------------------

  # Plot
  do.call(monitorPlot_timeseries, argsList)

  if ( FALSE ) {
    setModelDataDir('~/Data/Bluesky')
    bs_grid <- bluesky_load(model = "PNW-4km", modelRun = 2019100900)
    longitude <- bs_grid$longitude
    latitude <- bs_grid$latitude
    count = 1000
  }

}
