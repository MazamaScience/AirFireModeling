#' @keywords bs_grid, spaghettiPlot
#' @export
#' @importFrom PWFSLSmoke monitor_rollingMean monitor_timeseriesPlot
#' @importFrom graphics title
#' @title Create a Spaghetti Plot From ws_monitor Data
#' @param bs_grid a bs_grid list
#' @param longitude target longitude
#' @param latitude target latitude
#' @param count count of closest grid cells to be included in the plot
#' @param width rolling mean width in hours
#' @param ... additional arguments to pass to plot() via monitorPlot_timeseries()
#' 
#' @description Creates a "spaghetti plot" timeseries  with a line for each of
#' the \code{count} model grid cells closest to the target location. A centered
#' rolling mean is applied to each timeseries before plotting.
#' 
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/Bluesky')
#' 
#' bluesky <- bluesky_aggregate(
#'   model = 'PNW-4km', 
#'   firstModelRun = 20190915, 
#'   lastModelRun = 20190917,
#'   chunk = 1
#' )
#' gridPlot_spaghetti(bluesky, longitude=-117.400, latitude=47.660)
#' }

gridPlot_spaghetti <- function( 
  bs_grid = NULL,
  longitude = NULL,
  latitude = NULL,
  count = 9,
  width = 3,
  ... 
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(bs_grid)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  
  if ( length(longitude) > 1 ) 
    stop("Parameter 'longitude' must be of length 1")
  
  if ( length(latitude) > 1 ) 
    stop("Parameter 'latitude' must be of length 1")
  
  # ----- Set up arguments list ------------------------------------------------
  
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
  
  # Always use UTC to avoid multiple-timezone issues
  if ( !('add' %in% names(argsList)) ) {
    argsList$localTime <- FALSE
  }
  
  # ----- Prepare data ---------------------------------------------------------
  
  # Default radius = 50km should be big enough to get count grid cells
  radius <- 50000
  
  ws_monitors <- grid_subsetByDistance( 
    bs_grid,
    longitude = longitude,
    latitude = latitude,
    radius = radius,
    count = count 
  )
  
  rollingMeans <- PWFSLSmoke::monitor_rollingMean(ws_monitors, width)
  
  argsList$ws_monitor <- rollingMeans
  
  # ----- Create plot ----------------------------------------------------------
  
  # Plot
  do.call(PWFSLSmoke::monitor_timeseriesPlot, argsList)
  
  # Annotate
  graphics::title(sprintf(
    "%s: %s -- %d closest grid cells to %.3f, %.3f",
    bs_grid$model,
    bs_grid$modelRun,
    count,
    longitude, latitude
  ))
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaSpatialUtils)
  library(AirFireModeling)
  
  setModelDataDir('~/Data/Bluesky')
  
  PWFSLSmoke::initializeMazamaSpatialUtils()
  
  bs_grid = bluesky_aggregate(
    model = 'PNW-4km',
    firstModelRun = 20190915,
    lastModelRun = 20190917,
    chunk = 1
  )
  
  longitude = -117.400
  latitude = 47.660
  count = 9
  width = 3
  
}

