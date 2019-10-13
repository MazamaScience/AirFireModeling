#' @export
#' 
#' @title Subset a \emph{bs_grid} object by distance from a location 
#' 
#' @param bs_grid \emph{bs_grid} object.
#' @param longitude Target longitude from which the distance will be calculated.
#' @param latitude Target latitude from which the distance will be calculated.
#' @param radius Radius in meters.
#' @param count Number of grid cells to return.
#' 
#' @description This function subsets the \code{bs_grid} object and creates a
#' new object of type \code{ws_monitor} that is compatible with 
#' \pkg{PWFSLSmoke} \code{monitor_~} functions.
#' 
#' When \code{count} is used, a \code{ws_monitor} object is created containing 
#' \strong{up to} \code{count} monitors ordered by increasing distance from the 
#' target location. Only grid cells are found within \code{radius} will be 
#' returned even if this number is fewer than \code{count}.
#' 
#' @return \emph{ws_monitor} object where each unique monitor represents a
#' single grid cell from \code{bs_grid}.
#' 
#' @seealso \link{grid_subsetByMask}
#' 
#' @examples
#' \donttest{
#' # Next three lines required for bs_grid to ws_monitor conversion
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' # Now we can work as we normally would
#' setModelDataDir("~/Data/Bluesky")
#' bs <- bluesky_load(model = "PNW-1.33km", modelRun = 2019100900)
#' bs_monitors <- grid_subsetByDistance(bs, -121.017092, 46.901512, 
#'                                      radius = 10000)
#' 
#' # Now start using functions from PWFSLSMoke
#' bs_mean <- monitor_collapse(bs_monitors, monitorID = "mean", FUN = mean)
#' bs_median <- monitor_collapse(bs_monitors, monitorID = "median", FUN = median)
#' monitorPlot_timeseries(bs_monitors, style = "gnats", ylim = c(0, 100), cex = 1.5)
#' monitorPlot_timeseries(bs_median, type = "l", lwd = 3, col = "blue", add = TRUE)
#' monitorPlot_timeseries(bs_mean, type = "l", lwd = 3, col = "red", add = TRUE)
#' legend("topleft", col = c("red", "blue"), lwd = 1, legend = c("mean", "median"))
#' }

grid_subsetByDistance <- function(
  bs_grid = NULL, 
  longitude = NULL, 
  latitude = NULL, 
  radius = 20000, 
  count = NULL
) {
  
class  # ----- Subset grid ----------------------------------------------------------
  
  distanceGrid <- grid_distance(bs_grid, longitude, latitude)
  
  # Create grid mask
  gridMask <- distanceGrid <= radius
  
  # NOTE:  Subsetting by mask "unravels" the gridded data and returns an object 
  # NOTE:  of type ws_monitor.
  
  # Apply gridMask to bs_grid and distanceGrid
  ws_monitor <- grid_subsetByMask(bs_grid, gridMask)
  distanceVector <- as.numeric(distanceGrid[gridMask])
  
  # If requested, return only the grid cells closest to the target location.
  if ( !is.null(count) ) {
    
    # NOTE:  When using count, return monitors in distance order and make sure
    # NOTE:  that the distances are also subset and returned in distance order.
    
    count <- as.integer(count)
    withinRadiusCount <- nrow(ws_monitor$meta)
    
    if ( count > withinRadiusCount )
      count <- withinRadiusCount
    
    # Find the 'count' closest monitors
    closestIndices <- order(distanceVector)[1:count]
    
    # Subset distances and monitors
    distanceVector <- distanceVector[closestIndices]
    monitorIDs <- ws_monitor$meta$monitorID[closestIndices]
    ws_monitor <-  PWFSLSmoke::monitor_subset(
        ws_monitor, 
        monitorIDs = monitorIDs
      )
    
  }

  # ----- Return ---------------------------------------------------------------
  
  # Add 'distance' to the ws_monitor object
  ws_monitor[['distance']] <- distanceVector
  
  # Return the ws_monitor data list
  return(ws_monitor)
  
}
