#' @keywords bs_grid
#' @export
#' @title Subset a \code{bs_grid} Data List by Distance from a Location 
#' @param bs_grid bs_grid data list
#' @param longitude target longitude from which the radius will be calculated
#' @param latitude target latitude from which the radius will be calculated
#' @param radius radius in km
#' @param count number of grid cells to return
#' @description This function subsets the \code{bs_grid} data list and creates a
#' new object of type \code{ws_monitor} that is compatible with \code{monitor_~} functions.
#' 
#' When \code{count} is used, a \code{ws_monitor} object is created containing \strong{up to}
#' \code{count} monitors ordered by increasing distance from the target location. Only
#' grid cells are found within \code{radius} will be returned even if this number is 
#' fewer than \code{count}.
#' @return ws_monitor object where each gridcell is considered as a single monitoring station
#' @seealso \link{grid_subsetByMask}
#' @examples
#' \dontrun{
#' # Next three lines required for bs_grid-ws_monitor conversion
#' library(MazamaSpatialUtils)
#' setSpatialDataDir('~/Data/Spatial')
#' loadSpatialData('NaturalEarthAdm1')
#' # Now we can work as we normally do
#' setModelDataDir('~/Data/Bluesky')
#' bs <- bluesky_load(model="PNW-1.33km", modelRun=2016091200)
#' bs_monitors <- grid_subsetByDistance(bs, -121.017092, 46.901512, radius=10)
#' # Now start using functions from PWFSLSMoke
#' bs_mean <- monitor_collapse(bs_monitors, monitorID='mean', FUN=mean)
#' bs_median <- monitor_collapse(bs_monitors, monitorID='median', FUN=median)
#' monitorPlot_timeseries(bs_monitors, style='gnats', ylim=c(0,100), cex=1.5)
#' monitorPlot_timeseries(bs_median, type='l', lwd=3, col='blue', add=TRUE)
#' monitorPlot_timeseries(bs_mean, type='l', lwd=3, col='red', add=TRUE)
#' legend('topleft',col=c('red','blue'), lwd=1, legend=c('mean','median'))
#' }

grid_subsetByDistance <- function(bs_grid, longitude=NULL, latitude=NULL, radius=20, count=NULL) {
  
  # Sanity check 
  if ( ! "bs_grid" %in% class(bs_grid) ) {
    stop("bs_grid object is not of class 'bs_grid'.")
  }
  
  if ( ! length(dim(bs_grid$data[[1]])) == 3 ) {
    stop("Currently, only 3-D grids are supported.")
  }
  
  # Sanity check 
  if ( is.null(longitude) | is.null(latitude) ) {
    stop("Need to specify target coordinates.")
  }
  
  # Create grid of distance to target location
  distanceGrid <- grid_distance(bs_grid, longitude, latitude)
  
  # Create grid mask
  gridMask <- distanceGrid <= radius
  
  # NOTE:  Subsetting by mask "unravels" the gridded data and returns an object of type ws_monitor.
  
  # Apply gridMask to the bs_grid dataList.
  ws_monitor <- grid_subsetByMask(bs_grid, gridMask)
  distanceVector <- as.numeric(distanceGrid[gridMask])
  
  # If user specified the count, return only the gridcells closest to the target location.
  if ( ! is.null(count) ) {
    
    # NOTE:  When using count, return monitors in distance order and make sure
    # NOTE:  that the distances are also subset and returned in distance order.
    
    count <- as.integer(count)
    withinRadiusCount <- nrow(ws_monitor$meta)
    
    # Sanity check 
    if ( count > withinRadiusCount ) {
      count <- withinRadiusCount
    }
    
    # Find the 'count' closest monitors
    closestIndices <- order(distanceVector)[1:count]
    
    # Subset distances and monitors
    distanceVector <- distanceVector[closestIndices]
    ws_monitor <- PWFSLSmoke::monitor_subset(ws_monitor, monitorIDs=ws_monitor$meta$monitorID[closestIndices])
    
  }
  
  # Add 'distance' to the ws_monitor object
  ws_monitor[['distance']] <- distanceVector

  # Return the ws_monitor data list
  return(ws_monitor)
  
}
