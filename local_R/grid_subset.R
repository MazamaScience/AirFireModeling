# #' @keywords ws_grid
# #' @export
# #' @title Subset \code{ws_grid} Data List to a Smaller Grid
# #' @param ws_grid ws_grid data list
# #' @param xlim optional longitude range with lo and hi longitude values
# #' @param ylim optional latitude range with lo and hi longitude values
# #' @param tlim optional time range with lo and hi time values (POSIXct)
# #' @description The incoming gridded data list is filtered according to the parameters
# #' passed in.  If any parameter is not specified, that parameter will not be used in the filtering.
# #' @return bluesky data list with reduced dimensions
# #' @examples \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' bs <- bluesky_load(model="PNW-1.33km", modelRun=2016091000)
# #' xlim <- c(-118, -114)
# #' ylim <- c(45, 48)
# #' gridMap(bs, xlim=xlim, ylim=ylim, tMask=(1:24))
# #' }
# #' @seealso \link{grid_subsetByDistance}

# grid_subset <- function(ws_grid, xlim=NULL, ylim=NULL, tlim=NULL) {
  
#   # Sanity check
#   if ( !("ws_grid" %in% class(ws_grid)) ) {
#     stop("ws_grid object is not of class 'ws_grid'.")
#   }
  
#   # TODO:  Add support for 4D grids
#   if ( !(length(dim(ws_grid$data[[1]])) == 3) ) {
#     stop("Currently, only 3-D grids are supported.")
#   }
  
#   # Create masks based on lat, lon and time ranges
#   if ( is.null(xlim) ) {    
#     lonMask <- rep(TRUE,length(ws_grid$longitude))
#   } else {
#     lonMask <- ws_grid$longitude >= xlim[1] & ws_grid$longitude <= xlim[2]
#   }
  
#   if ( is.null(ylim) ) {
#     latMask <- rep(TRUE,length(ws_grid$latitude))
#   } else {
#     latMask <- ws_grid$latitude >= ylim[1] & ws_grid$latitude <= ylim[2]
#   }
  
#   if ( is.null(tlim) ) {
#     tMask <- rep(TRUE,length(ws_grid$time))
#   } else {
#     tMask <- ws_grid$time >= tlim[1] & ws_grid$time <= tlim[2]
#   }
  
#   # Subset using masks
#   ws_grid$longitude <- ws_grid$longitude[lonMask]
#   ws_grid$latitude <- ws_grid$latitude[latMask]
#   ws_grid$time <- ws_grid$time[tMask]
#   for (param in names(ws_grid$data)) {
#     ws_grid$data[[param]] <- ws_grid$data[[param]][lonMask, latMask, tMask]
#   }
  
#   return(ws_grid)
  
# }
