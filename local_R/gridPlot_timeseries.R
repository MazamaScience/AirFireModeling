# #' @keywords ws_grid
# #' @export
# #' @title Timeseries Plot for a `ws_grid` Object
# #' @param ws_grid 'gridded' BlueSky DataList
# #' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
# #' @param localTime A logical whether you are going to use GMT time or not
# #' @param shadedNight A logical whether you want to add shaded rectangles for the local night times
# #' @param add logical specifying whether to add points to an existing plot
# #' @param gridPos position of grid lines either 'over', 'under' ('' for no grid lines)
# #' @param gridCol grid line color
# #' @param gridLwd grid line width
# #' @param gridLty grid line type
# #' @param dayLwd day marker line width
# #' @param hourLwd hour marker line width
# #' @param hourInterval interval for grid (max=12)
# #' @param title The desired title of your plot. Setting as NULL prints out the default title.
# #' @param ylim the y limits of the plot
# #' @description This funciton plot timeseries from an 'unraveled' BlueSky dataList.
# #' @seealso \link{grid_subsetByDistance}
# #' @examples
# #' \dontrun{
# #'   setModelDataDir('~/Data/Bluesky')
# #'   ws_grid <- bluesky_load(model="PNW-1.33km", modelRun=2016091100)
# #'   ws_grid <- grid_subset(ws_grid, c(-123,-120), c(47,48.5))
# #'   gridMap(ws_grid)
# #'   gridPlot_timeseries(ws_grid, tlim=c(20160911, 20160912))
# #' }

# gridPlot_timeseries <- function(ws_grid,
#                                 tlim=NULL,
#                                 localTime=FALSE,
#                                 shadedNight=FALSE,
#                                 add=FALSE,
#                                 gridPos='', gridCol='black', gridLwd=1, gridLty='solid',
#                                 dayLwd=0, hourLwd=0, hourInterval=6,
#                                 title=NULL, ylim=NULL) {

#   # Sanity Check
#   if ( ! length(dim(ws_grid$data[[1]])) == 3 ) {
#     stop("Currently, only 3-D grids are supported.")
#   }
  
#   # Timezone information
#   targetLon <- stats::median(ws_grid$longitude)
#   targetLat <- stats::median(ws_grid$latitude)
#   timezone <- MazamaSpatialUtils::getTimezone(targetLon, targetLat, useBuffering=TRUE)
#   # convert to local time if requested
#   if ( localTime ) {
#     ws_grid$time <- lubridate::with_tz(ws_grid$time, tzone=timezone)
#   }
  
#   # tlim application
#   if ( !is.null(tlim) ) {
#     # When tlim is specified in whole days we add hours to get the requsted full days
#     tlimStrings <- as.character(tlim)
#     if ( stringr::str_length(tlimStrings)[1] == 8 ) {
#       tlim[1] <- paste0(tlim[1],'00')
#     }
#     if ( stringr::str_length(tlimStrings)[2] == 8 ) {
#       tlim[2] <- paste0(tlim[2],'23')
#     }
    
#     # convert tlim to POSIXct in appropriate timezone
#     if ( localTime ) {
#       tlim <- parseDatetime(tlim, timezone)
#     } else {
#       tlim <- parseDatetime(tlim)
#     }
    
#     ws_grid <- grid_subset(ws_grid, tlim=tlim)
    
#   }
  
#   # Set the time axis to use for plotting
#   times <- ws_grid$time

#   # TODO:  improve naming of 'breaks' to better explain what is going on

#   # Create histogram matrix
#   myFun <- function(x) { return( table(cut(x, breaks=c(-1e12,seq(0,5000,10),1e12))) ) }
#   histMatrix <- apply(ws_grid$data$pm25, 3, myFun)

#   # Y values associated with the matrix derived from breaks above
#   y_values <- seq(-10,5010,10)

#   # Image breaks and colors
#   breaks <- c(-1e12,0,1,2,5,10,20,50,100,200,500,1000,1e12)
#   colors <- c('transparent', rep('black',length(breaks)-2))

#   # Set opacity based on relative number in each grid cell
#   normalization <- stats::quantile(as.numeric(histMatrix),0.99)
#   for (i in 2:length(colors)) {
#     alpha <- breaks[i+1] / normalization
#     colors[i] <- grDevices::adjustcolor(colors[i], alpha)
#   }

#   # y limit
#   if ( is.null(ylim) ) {
#     ylim <- c(0, 1.1 * max(as.numeric(ws_grid$data$pm25), na.rm=TRUE))
#   }

#   # ---------- Plotting -------
#   par(mar=c(4,6,4,0)+0.1)
  
#   if ( is.null(title) ) {
#     dims <- dim(ws_grid$data$pm25)
#     cellCount <- dims[1] * dims[2] * dims[3]
#     title <- paste0("Timeseries Plot of ",cellCount, " PM2.5 values in gridded region")
#   }
  
#   image(ws_grid$time, y_values, t(histMatrix), las=1, col=c(rep('transparent',length(breaks)-1)), breaks=breaks,
#         axes=FALSE, xlab='', ylab='PM2.5',
#         ylim=ylim, add=add, main=title)
  
#   # Add vertical lines to denote days and/or hour breaks
#   hour_indices <- which(as.numeric(strftime(times,format="%H",tz=timezone)) %% hourInterval == 0)
#   day_indices <- which(as.numeric(strftime(times,format="%H",tz=timezone)) %% 24 == 0)
#   abline(v=times[hour_indices], lwd=hourLwd) # at beginning of hour
#   abline(v=times[day_indices], lwd=dayLwd) # at beginning of day

#   # Add horizontal grid lines (before points if grid=='under')
#   if ( gridPos == 'under' ) {
#     abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
#   }
  
#   image(ws_grid$time, y_values, t(histMatrix), las=1, col=colors, breaks=breaks, axes=FALSE, add=TRUE)

#   # Add horizontal grid lines (after points if grid=='over')
#   if ( gridPos == 'over' ) {
#     abline(h=axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
#   }
  
#   # Y axis
#   axis(2, las=1)

#   # Nicely formatted time axis
#   if ( !localTime ) {
#     at1 <- times[seq(1,length(times),6)]
#     labels1 <- strftime(at1, "%H",  tz = "UTC")
#     axis.POSIXct(1, times, at=at1, labels=labels1, tcl = -0.25, mgp = c(1,0.5,0))
#   } else {
#     at1 <- times[seq(1,length(times),6)]
#     labels1 <- strftime(at1, "%H")
#     axis.POSIXct(1, times, at=at1, labels=labels1, tcl = -0.25, mgp = c(1,0.5,0))
#   }

#   # Additional tickless axis for locating noons on the time axis and label them as the Dayname,Month Day
#   noons <- times[lubridate::hour(times) == 12]
#   at2 <- noons

#   if ( !localTime ) {
#     labels2 <- strftime(at2, "%a, %b %d UTC")
#     axis.POSIXct(1, times, at=at2, labels=labels2, tick = FALSE, line = 1)
#   } else {
#     labels2 <- strftime(at2, "%a, %b %d %Z")
#     axis.POSIXct(1, times, at=at2, labels=labels2, tick = FALSE, line = 1)
#   }

#   if ( shadedNight ) {
#     timeInfo <- timeInfo(ws_grid$time, targetLon, targetLat)
#     PWFSLSmoke::addShadedNight(timeInfo)
#   }

# }
