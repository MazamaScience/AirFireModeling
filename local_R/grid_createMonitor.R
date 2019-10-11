# #' @keywords ws_monitor
# #' @export
# #' @title Collapse a ws_grid object into a ws_monitor Object with a Single Monitor
# #' @param ws_grid data list of class \code{ws_grid}
# #' @param longitude custom longitude of the collapsed monitoring station. (Default = median of the longitudes)
# #' @param latitude custom latitude of the collapsed monitoring station. (Default = median of the latitudes)
# #' @param monitorID The custom monitorID of the collapsed monitoring station. (Default = 'generated_id')
# #' @param radius radius in km
# #' @param count The number of closest gridcells around the target location. Set to NULL by default.
# #' @param FUN The function to collapse all data at a single time index. (Default = mean)
# #' @param ... Any additional argument to be passed on to the apply() function, e.g. \code{na.rm=TRUE}
# #' @description When given a ws_grid object, the function collapses all the data into a single timeseries using the function
# #' provided in the FUN argument.
# #' 
# #' This function combines the operations of \code{grid_subsetByDistancee} and \code{monitor_collapse}.
# #' @return A ws_monitor object representing a single collapsed monitor
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' # TODO:  figure out what's wrong with this example. Why are calculated distances so huge?
# #' bs1 <- bluesky_aggregate('PNW-4km', 20160910, 20160913, chunk=1)
# #' bs1_monitor <- grid_createMonitor(bs1, radius=5, monitorID='mean', FUN=mean, na.rm=TRUE)
# #' monitorPlot_timeseries(bs1_monitor)
# #' } 

# grid_createMonitor <- function(ws_grid, longitude=stats::median(ws_grid$longitude), latitude=stats::median(ws_grid$latitude),
#                               radius=20, count=NULL,
#                               monitorID='generated_id',
#                               FUN=mean, ...) {
  
#   # Sanity check 
#   if (is.null(longitude) | is.null(latitude)) {
#     stop("Required parameter 'longitude' or 'latitude' is missing.")
#   }
  
#   if ( is.null(count) ) {
#     ws_monitor <- grid_subsetByDistance(ws_grid, longitude, latitude, radius)
#   } else {
#     ws_monitor <- grid_subsetByDistance(ws_grid, longitude, latitude, radius, count)
#   }
  
#   ws_monitor <- monitor_collapse(ws_monitor,
#                                  longitude=longitude,
#                                  latgitude=latitude,
#                                  FUN=FUN, 
#                                  monitorID = monitorID,
#                                  ...)
  
#   return(ws_monitor)
  
# }
  
