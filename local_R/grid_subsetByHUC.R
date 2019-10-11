# #' @keywords ws_grid
# #' @export
# #' @title Subset a \code{ws_grid} Data List by Hydrologic Unit Code
# #' @param ws_grid ws_grid data list
# #' @param HUC hydrologic unit code
# #' @description This function subsets the \code{ws_grid} data list and creates a
# #' new object of type \code{ws_monitor} that is compatible with \code{monitor_~} functions.
# #' @details Use of this function requires installation of the \code{WBDHU#.RData} files that
# #' can be generated with the \pkg{MazamaSpatialUtils} package. The appropriate data file
# #' matching the level of the \code{HUC} argument must be loaded as in the example.
# #' @return ws_monitor object where each gridcell is considered as a single monitoring station
# #' @seealso \link{grid_subsetByMask}
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' bs <- bluesky_load(model="PNW-1.33km", modelRun=2016091200)
# #'
# #' setSpatialDataDir('~/Data/Spatial')
# #' loadSpatialData('WBDHU10')
# #' WA10 <- subsetHUC(WBDHU10, allStateCodes='WA')
# #'
# #' longitude <- -123.801
# #' latitude <- 47.704
# #' HUC <- MazamaSpatialUtils::getHUC(longitude, latitude, WA10)
# #'
# #' QueetsRiver <- grid_subsetByHUC(bs, HUC)
# #' monitorGoogleMap(QueetsRiver, cex=0.5)
# #' }

# grid_subsetByHUC <- function(ws_grid, HUC=NULL) {
  
#   # Sanity check
#   if ( !"ws_grid" %in% class(ws_grid) ) {
#     stop("ws_grid object is not of class 'ws_grid'.")
#   }
  
#   # Sanity check
#   if ( !length(dim(ws_grid$data[[1]]))==3 ) {
#     stop("Currently, only 3-D grids are supported.")
#   }
  
#   # Sanity check
#   if ( is.null(HUC) || (length(HUC) > 1) ) {
#     stop("Need to specify a single HUC.")
#   } else {
#     HUC <- as.character(HUC)
#   }
  
#   # Sanity check
#   WBDName <- paste0('WBDHU',stringr::str_length(HUC))
#   if ( !exists(WBDName) ) {
#     stop(paste0('Please load ',WBDName,' with "loadSpatialData(\'',WBDName,'\')"'))
#   }
  
#   # Subset model grid based on HUC bounds
#   wbd <- get(WBDName)
#   wbd <- wbd[wbd$HUC==HUC,]
#   wbd_bbox <- sp::bbox(wbd)
#   ws_grid <- grid_subset(ws_grid,xlim = wbd_bbox[1,], ylim = wbd_bbox[2,])
  
#   # Create unraveled grids of lons and lats
#   allLons <- rep(ws_grid$longitude, times = length(ws_grid$latitude))
#   allLats <- rep(ws_grid$latitude, each = length(ws_grid$longitude))
  
#   # Find the HUC at each location and test against the 'HUC' argument
#   allHUCs <- MazamaSpatialUtils::getHUC(allLons, allLats, wbd) #TODO: check if this step can be sped up
#   allHUCs[is.na(allHUCs)] <- FALSE
#   gridMask <- allHUCs == HUC
  
#   # Create 2D gridded version of the mask
#   gridMask <- matrix(gridMask, nrow=length(ws_grid$longitude), ncol=length(ws_grid$latitude), byrow=FALSE)
  
#   # NOTE:  Subsetting by mask "unravels" the gridded data and returns an object of type ws_monitor.
  
#   # Apply gridMask to the ws_grid dataList
#   ws_monitor <- grid_subsetByMask(ws_grid, gridMask)
  
#   # Add 'HUC' to this list
#   ws_monitor[['HUC']] <- HUC
  
#   # Return the ws_monitor data list
#   return(ws_monitor)
  
# }
