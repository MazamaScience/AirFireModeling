# #' @keywords ws_grid, ws_monitor
# #' @export
# #' @title Correlation Map
# #' @param ws_grid ws_grid data list
# #' @param ws_monitor monitor data list
# #' @param monitorID monitor ID for correlation to model data
# #' @param param data parameter to plot
# #' @param xlim longitude range to be plotted
# #' @param ylim latitude range to be plotted
# #' @param tlim time range tover which to calculation the correlation
# #' @param breaks used by \code{image()} function to partition correlation values into different colors
# #' @param colors vector of colors to use
# #' @param zoom range extension as a fraction of the coordinate range
# #' @description Plots a map of the correlation of bluesky model data with a monitoring timeseries.
# #' 
# #' When \code{colors=NULL}, a color palette is given by \code{colorRampPalette}.
# #' @seealso \link{gridMap}
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' ws_grid <- bluesky_load(model="PNW-1.33km", modelRun=2016091200)
# #' ws_monitor <- airsis_createMonitorObject(20160912, 20160913, "USFS", 1033)
# #' gridMap_correlation(ws_grid, ws_monitor)
# #' }

# gridMap_correlation <- function(ws_grid, ws_monitor, monitorID=NULL, param="pm25",
#                                 xlim=NULL, ylim=NULL,tlim=NULL,
#                                 breaks=c(-1,-.7,-.5,-.3,-.1,.1,.3,.5,.7,1),
#                                 colors=rev(RColorBrewer::brewer.pal(9,'RdBu')), zoom=1.5) {
  
#   if ( !"ws_grid" %in% class(ws_grid) ) {
#     stop("ws_grid object is not of class 'ws_grid'.")
#   }
  
#   if ( !"ws_monitor" %in% class(ws_monitor) ) {
#     stop("ws_monitor object is not of class 'ws_monitor'.")
#   }
  
#   if ( !(length(dim(ws_grid$data[[1]])) == 3) ) {
#     stop("Currently, only 3-D grids are supported.")
#   }
  
#   if ( ncol(ws_monitor$data) > 2 ) {
#     if ( is.null(monitorID) ) {
#       ws_monName <- deparse(substitute(ws_monitor))
#       stop(paste0("ws_monitor may only contain data from a single monitor. Please provide a monitorID from ",ws_monName,"$meta$monitorID"))
#     } else {
#       if ( monitorID %in% ws_monitor$meta$monitorID ) {
#         ws_monitor <- monitor_subset(ws_monitor,monitorIDs=monitorID)        
#       } else {
#         stop(paste0("monitorID ",monitorID," not found in ws_monitor meta data"))
#       }
#     } 
#   } else {
#     monitorID <- ws_monitor$meta$monitorID
#   }
  
#   # tlim handling
#   if ( !is.null(tlim) ) {
#     tlim <- as.character(tlim)
#     if ( nchar(tlim)[1]==8 ) {
#       tlim[1] <- paste0(tlim[1],"00")
#     }
#     if ( nchar(tlim)[2]==8 ) {
#       tlim[2] <- paste0(tlim[2],"23")
#     }
#     tlim <- parseDatetime(tlim)
#   }
  
#   tlimBeg <- max(ws_grid$time[1],ws_monitor$data$datetime[1],tlim[1])
#   tlimEnd <- min(ws_grid$time[length(ws_grid$time)],ws_monitor$data$datetime[length(ws_monitor$data$datetime)],tlim[2])
#   tlim <- c(tlimBeg,tlimEnd)
  
#   # set xlim
#   if ( length(xlim)==2 ) {
#     xlim <- adjustRange(xlim, zoom)
#   } else {
#     if ( is.null(xlim) ) {
#       xmid <- ws_monitor$meta$longitude
#     } else {
#       xmid <- xlim
#     }
#     xdiff <- 1/zoom
#     xlim <- c(max(range(ws_grid$longitude)[1],xmid-4*xdiff),min(range(ws_grid$longitude)[2],xmid+4*xdiff))
#   }
  
#   # set ylim
#   if ( length(ylim)==2 ) {
#     ylim <- adjustRange(ylim, zoom)
#   } else {
#     if ( is.null(ylim) ) {
#       ymid <- ws_monitor$meta$latitude
#     } else {
#       ymid <- ylim
#     }
#     ydiff <- 1/zoom
#     ylim <- c(max(range(ws_grid$latitude)[1],ymid-4*ydiff),min(range(ws_grid$latitude)[2],ymid+4*ydiff))
#   }
  
#   # subset ws_grid if necessary
#   ws_gridSub <- grid_subset(ws_grid, xlim, ylim, tlim)
#   lon <- ws_gridSub$longitude
#   lat <- ws_gridSub$latitude
#   gridData <- ws_gridSub$data[[param]][,,]
  
#   # Subset ws_monitor to have the same times as ws_grid
#   ws_monSub <- monitor_subset(ws_monitor, tlim=tlim)
#   monitorData <- ws_monSub$data[,2]
  
#   # Calculation the correlation
#   # NOTE:  Suppress warnings to avoid "standard deviation is zero" for model grid cells with no smoke
#   gridSlice <- suppressWarnings( apply(gridData, 1:2, stats::cor, monitorData, "complete.obs") )
  
#   # style options
#   # create default palette if none is passed in
#   if (is.null(colors)) {
#     palette <- colorRampPalette(c("white", "orange", "firebrick"))
#     colors <- palette(length(breaks)-1)
#   }
  
#   # create map
#   maps::map('state', xlim=xlim, ylim=ylim)
#   title(main=paste0("Correlation of ", monitorID," vs. ", ws_grid$model, "\n", tlim[1], " to ",tlim[2]))
#   image(lon, lat, gridSlice, breaks=breaks, col=colors, add=TRUE)
#   maps::map('county', col='gray80', xlim=xlim, ylim=ylim, add=TRUE)
#   maps::map('state', xlim=xlim, ylim=ylim, add=TRUE)
#   maps::map.axes()

#   labels <- rev(paste0(lag(breaks),' to ',breaks)[-1])
#   legend("bottomright", col=rev(colors), legend=labels, pch=16, cex=.7)
  
#   addBullseye(ws_monSub$meta$longitude,ws_monSub$meta$latitude)
  
# }
