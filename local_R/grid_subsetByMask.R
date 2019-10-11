# #' @keywords ws_grid
# #' @export
# #' @title Subset a \code{ws_grid} Data List with a Grid Mask 
# #' @param ws_grid ws_grid data list
# #' @param gridMask logical 2D matrix
# #' @param countryCodes vector of country codes covered by the grid
# #' @description This function subsets the \code{ws_grid} data list and 
# #' restructures the data into a \code{ws_monitor} data list appropriate
# #' for use with various monitor_~() functions.
# #' @return ws_monitor object of the masked bluesky data
# #' @seealso \link{grid_subsetByDistance} 
# #' @examples
# #' \dontrun{
# #' # Next three lines required for ws_grid-ws_monitor conversion
# #' library(MazamaSpatialUtils)
# #' setSpatialDataDir('~/Data/Spatial')
# #' loadSpatialData('NaturalEarthAdm1')
# #' # Now we can work as we normally do
# #' setModelDataDir('~/Data/Bluesky')
# #' bs <- bluesky_load(model="PNW-1.33km", modelRun=2016091200)
# #' distanceMatrix <- grid_distance(bs, -121.017092, 46.901512)
# #' radius <- 20
# #' gridMask <- distanceMatrix <= radius
# #' bs_monitors <- grid_subsetByMask(bs, gridMask)
# #' monitorGoogleMap(bs_monitors, cex=.75)
# #' }

# grid_subsetByMask <- function(ws_grid, gridMask, countryCodes=c('CA','US')) {
  
#   # Sanity check 
#   if ( !"ws_grid" %in% class(ws_grid) ) {
#     stop("ws_grid object is not of class 'ws_grid'.")
#   }
  
#   if ( !length(dim(ws_grid$data[[1]])) == 3 ) {
#     stop("Currently, only 3-D grids are supported.")
#   }
  
#   # Sanity check
#   if ( length(ws_grid$longitude) != nrow(gridMask) || length(ws_grid$latitude) != ncol(gridMask) ) {
#     stop(paste0('ws_grid and gridMask have different dimensions.'))
#   }
  
#   cellCount <- sum(gridMask)
  
#   # Create longitudes and latitudes associated with each grid cell
#   allLons <- rep(ws_grid$longitude, times=length(ws_grid$latitude))
#   allLats <- rep(ws_grid$latitude, each=length(ws_grid$longitude))
#   lon2D <- matrix(allLons, nrow=length(ws_grid$longitude), ncol=length(ws_grid$latitude), byrow=FALSE)
#   lat2D <- matrix(allLats, nrow=length(ws_grid$longitude), ncol=length(ws_grid$latitude), byrow=FALSE)
  
#   # Apply mask to subset and 'unravel'
#   latitude <- lat2D[gridMask]
#   longitude <- lon2D[gridMask]
  
#   # Construct a monitorID for each grid cell in our 'unraveled' vector
#   monitorID <- paste(round(longitude, 2), round(latitude, 2), sep="_")
  
#   # Pick the middle index for the target lon and lat to use in the timezone calculation
#   targetLon <- longitude[cellCount/2]
#   targetLat <- latitude[cellCount/2]
#   timezone <-  MazamaSpatialUtils::getTimezone(targetLon, targetLat, useBuffering=TRUE)
  
#   # Construct the meta dataframe
#   meta <- data.frame(monitorID=monitorID,
#                      longitude=longitude,
#                      latitude=latitude,
#                      stringsAsFactors=FALSE)
#   rownames(meta) <- meta$monitorID
  
#   # Add timezone, stateCode and countryCode
#   meta <- PWFSLSmoke::addMazamaMetadata(meta, lonVar='longitude', latVar='latitude',
#                                         countryCodes=countryCodes)
  
#   # Mask/unravel 3D pm25 array and put it back onto a 2D grid (cell by T)
#   unraveledPM25 <- ws_grid$data[['pm25']][gridMask]
  
#   # NOTE:  PM25 3D data are unraveled by X then Y and finally by T
#   # NOTE:  We will put this into a matrix organized as col=gridCell and row=time (XY by T),
#   # NOTE:  then convert to a dataframe by including a datetime column, as we do with monitor data
  
#   pm25Matrix <- matrix(unraveledPM25, nrow=length(ws_grid$time), ncol=cellCount, byrow=TRUE)
#   pm25 <- data.frame(datetime=ws_grid$time, pm25Matrix)
#   colnames(pm25) <- c('datetime',monitorID)
  
#   # TODO:  ws_monitor$data <- list(pm25=pm25)
  
#   # Create the 'ws_monitor' data list
#   ws_monitor <- list(meta=meta,
#                      data=pm25)
  
#   # Carry over all other singleton information (typically model run metadata)
#   for (name in names(ws_grid)) {
#     # NOTE:  ws_grid$time has class = c("POSIXct","POSIXt") so we only compare the first one
#     if (class(ws_grid[[name]])[1] %in% c('character','numeric','POSIXct')) {
#       if (length(ws_grid[[name]] == 1)) {
#         ws_monitor[[name]] <- ws_grid[[name]]
#       }
#     }
#   }  
  
#   return(structure(ws_monitor, class = c("ws_monitor", "list")))
  
# }




