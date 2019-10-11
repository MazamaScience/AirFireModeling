# #' @keywords ws_grid
# #' @export
# #' @import maps mapproj
# #' @import graphics
# #' @importFrom grDevices colorRampPalette
# #' @title Map Gridded Data
# #' @param ws_grid ws_grid data list
# #' @param param data parameter to plot
# #' @param xlim longitude range to be plotted
# #' @param ylim latitude range to be plotted
# #' @param tMask logical time mask to select specific times
# #' @param slice either a time index or a function used to collapse the time axis
# #' @param style custom styling, one of \code{'default'}
# #' @param breaks used by \code{image()} function to partition values into different colors
# #' @param colors vector of colors to use
# #' @param addAxes logical specifying whether to add axes
# #' @param main title for the plot
# #' @param ... additional arguments passed to maps::map()
# #' @description Plots a map of bluesky data in a given area.
# #' 
# #' When a named \code{style} is used, some graphical parameters will be overridden. Available styles include:
# #' 
# #' \itemize{
# #' \item{\code{default}}{-- hourly values are individually colored by 24-hr AQI levels}
# #' }
# #'
# #' If \code{breaks} and \code{colors} are defined, they will override \code{style}.
# #' 
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' bs <- bluesky_load(model="PNW-1.33km", modelRun=2016091000)
# #' xlim <- c(-118, -114)
# #' ylim <- c(45, 48)
# #' gridMap(bs, xlim=xlim, ylim=ylim, tMask=(1:40))
# #' }

# gridMap <- function(ws_grid, param="pm25",
#                     xlim=range(ws_grid$longitude), ylim=range(ws_grid$latitude), 
#                     tMask=rep(TRUE, times=length(ws_grid$time)), slice=get('max'),
#                     style='default', breaks=NULL, colors=NULL, addAxes=TRUE, main=NULL, ...) {
  
#   # initial checks
#   if ( !("ws_grid" %in% class(ws_grid)) ) {
#     stop("ws_grid object is not of class 'ws_grid'.")
#   }
  
#   if ( !(length(dim(ws_grid$data[[1]])) == 3) ) {
#     stop("Currently, only 3-D grids are supported.")
#   }
  
#   # subset bs grid if necessary
#   ws_gridSub <- grid_subset(ws_grid, xlim, ylim)
#   lon <- ws_gridSub$longitude
#   lat <- ws_gridSub$latitude
#   dataGrid <- ws_gridSub$data[[param]][,,tMask]
#   timeRange <- range(ws_grid$time[tMask])
  
#   # specify which map slice to plot, or how to aggregate values across time
#   if ( is.null(slice) ) {
#     stop("Need to specify a slice")
#   } else if ( class(slice) == "function" ) {
#     gridSlice <- apply(dataGrid, 1:2, slice)
#   } else if ( class(slice) == "integer" || class(slice) == "numeric" ) {
#     gridSlice <- dataGrid[,,as.integer(slice)]
#   } else {
#     stop("Improper use of slice argument")
#   }
  
#   # new breaks logic
#   if ( is.null(breaks) && is.null(colors) ) {
#     if ( style == 'default' ) {
#       # NOTE: Colors and breaks are the same as those used at https://www.airfire.org/data/bluesky-daily/
#       breaks <- c(1,5,10,20,40,90,140,350,525)
#       breaks <- c(min(c(0,gridSlice),na.rm=TRUE),breaks)
#       breaks <- c(breaks,max(c(600,gridSlice),na.rm=TRUE))
#       red <- c(255,255,255,255,255,255,255,200,150)/255
#       green <- c(225,195,165,135,105,75,46,2,3)/255
#       blue <- c(225,195,165,135,105,75,45,3,3)/255
#       colors <- c('transparent',grDevices::rgb(red=red, green=green, blue=blue))
#     } else {
#       if ( all(breaks==AQI$breaks_24) ) {
#         if ( length(breaks) != (length(colors) + 1) ) {
#           stop(paste0("Length of breaks [",length(breaks),"] must be one more than lenght of colors [",length(colors),"]"))
#         }
#       }
#     }
#   }
  
#   # create map
#   maps::map('state', xlim=xlim, ylim=ylim, ...)
#   image(lon, lat, gridSlice, breaks=breaks, col=colors, add=TRUE)
#   if ( is.null(main) ) {
#     title(main=paste0(ws_grid$model, "\n", timeRange[1], " to ",timeRange[2], " UTC"))
#   } else {
#     title(main=main)
#   }
#   result <- try( maps::map('county', col='gray80', xlim=xlim, ylim=ylim, add=TRUE),
#                  silent=TRUE )
#   result <- try( maps::map('state', xlim=xlim, ylim=ylim, add=TRUE),
#                  silent=TRUE )
#   if ( addAxes ) {
#     maps::map.axes()
#   } else {
#     box()
#   }
# }
