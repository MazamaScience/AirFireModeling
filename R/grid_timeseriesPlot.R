#' @keywords bs_grid
#' @export
#' @title Timeseries Plot for a `bs_grid` Object
#' @param bs_grid 'gridded' BlueSky DataList
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH])
#' @param localTime A logical whether you are going to use GMT time or not
#' @param shadedNight A logical whether you want to add shaded rectangles for the local night times
#' @param add logical specifying whether to add points to an existing plot
#' @param gridPos position of grid lines either "over", "under" ("" for no grid lines)
#' @param gridCol grid line color
#' @param gridLwd grid line width
#' @param gridLty grid line type
#' @param dayLwd day marker line width
#' @param hourLwd hour marker line width
#' @param hourInterval interval for grid (max=12)
#' @param title The desired title of your plot. Setting as NULL prints out the default title.
#' @param ylim the y limits of the plot
#' 
#' @description This funciton plot timeseries from an 'unraveled' BlueSky dataList.
#' 
#' @seealso \link{grid_subsetByDistance}
#' @examples
#' \dontrun{
#' library(MazamaSpatialUtils)
#' library(AirFireModeling)
#' setModelDataDir("~/Data/Bluesky")
#' 
#' # Full grid -- 72 million grid cells!
#' bs_grid <- bluesky_load(model = "PNW-1.33km", modelRun = 2019100900)
#' grid_timeseriesPlot(bs_grid, ylim = c(0, 500))
#' 
#' # Small region
#' bs_grid %>%
#'   grid_subset(xlim = c(-120, -118), ylim = c(45, 57)) %>%
#'   grid_timeseriesPlot(ylim = c(0, 500), shadedNight = TRUE, localTime = TRUE)
#' }

grid_timeseriesPlot <- function(
  bs_grid = NULL,
  tlim = NULL,
  localTime = FALSE,
  shadedNight = FALSE,
  add = FALSE,
  gridPos = "",
  gridCol = "black",
  gridLwd = 1,
  gridLty = "solid",
  dayLwd = 0,
  hourLwd = 0,
  hourInterval = 6,
  title = NULL,
  ylim = NULL 
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(bs_grid)
  
  if ( length(dim(bs_grid$data[[1]])) != 3 )
    stop("Currently, only 3-D grids are supported.")
  
  if ( !exists("SimpleTimezones") )
    stop(paste0(
      "MazamaSpatialUtils has not been loaded. Please:\n\n",
      "  library(MazamaSpatialUtils)\n\n"
    ))
  
  # ----- Handle time information ----------------------------------------------
  
  # Get the timezone of the median location
  targetLon <- stats::median(bs_grid$longitude)
  targetLat <- stats::median(bs_grid$latitude)
  timezone <- MazamaSpatialUtils::getTimezone(targetLon, targetLat, useBuffering = TRUE)
  
  # Convert to local time if requested
  if ( localTime ) {
    bs_grid$time <- lubridate::with_tz(bs_grid$time, tzone = timezone)
  }
  
  # Apply 'tlim'
  if ( !is.null(tlim) ) {
    if ( localTime ) {
      tlim <- MazamaCoreUtils::parseDatetime(tlim, timezone = timezone)
    } else {
      tlim <- MazamaCoreUtils::parseDatetime(tlim, timezone = "UTC")
    }
    bs_grid <- grid_subset(bs_grid, tlim=tlim)
  }
  
  # ----- Prepare data ---------------------------------------------------------
  
  # NOTE:  Instead of plotting tens of thousands of individual points, most
  # NOTE:  of which will be overplotted, we will create a matrix containg the
  # NOTE:  count of points that *would have* been drawn in each cell. These will
  # NOTE:  then be colored by that count to give a plot that looks similar to
  # NOTE:  one plotting all points with partial opacity.
  
  # Create histogram matrix
  FUN <- function(x) {
    return( table(cut(x, breaks=c(-1e12, seq(0,5000,10), 1e12))) )
  }
  histMatrix <- apply(bs_grid$data$pm25, 3, FUN)
  
  # Y values associated with the matrix derived from breaks above
  y_values <- seq(-10, 5010, 10)
  
  # Image breaks and colors
  breaks <- c(-1e12, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 1e12)
  colors <- c("transparent", rep("black", length(breaks)-2))
  norm <- stats::quantile(histMatrix, 0.99)
  
  for ( i in 2:length(colors) ) {
    alpha <- breaks[i+1] / norm
    colors[i] <- grDevices::adjustcolor(colors[i], alpha)
  }
  
  # ----- Plot style -----------------------------------------------------------
  
  if ( is.null(title) ) {
    dims <- dim(bs_grid$data$pm25)
    cellCount <- dims[1] * dims[2] * dims[3]
    title <- paste0("Timeseries Plot of ", 
                    format(cellCount, scientific = FALSE, big.mark = ","), 
                    " PM2.5 values in gridded region")
  }
  
  # y limit
  if ( is.null(ylim) ) {
    ylim <- c(0, 1.1 * max(as.numeric(bs_grid$data$pm25), na.rm = TRUE))
  }
  
  # Time axis to use for plotting
  time <- bs_grid$time
  
  # ----- Create plot ----------------------------------------------------------
  
  graphics::par(mar=c(4, 6, 4, 0) + 0.1)
  
  # Use base image
  graphics::image( x = bs_grid$time,
                   y = y_values,
                   z = t(histMatrix),
                   las = 1,
                   col = c(rep("transparent",length(breaks)-1)),
                   breaks = breaks,
                   axes = FALSE,
                   xlab = "",
                   ylab = "PM2.5",
                   ylim = ylim,
                   add = add,
                   main = title )
  
  # Add vertical lines to denote days and/or hour breaks
  hour_indices <- which(as.numeric(strftime(time,format="%H",tz=timezone)) %% hourInterval == 0)
  day_indices <- which(as.numeric(strftime(time,format="%H",tz=timezone)) %% 24 == 0)
  graphics::abline(v=time[hour_indices], lwd=hourLwd) # at beginning of hour
  graphics::abline(v=time[day_indices], lwd=dayLwd) # at beginning of day
  
  # Add horizontal grid lines (before points if grid=="under")
  if ( gridPos == "under" ) {
    graphics::abline(h = graphics::axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }
  
  graphics::image( 
    x = bs_grid$time,
    y = y_values,
    z = t(histMatrix),
    las = 1,
    col = colors,
    breaks = breaks,
    axes = FALSE,
    add = TRUE 
  )
  
  # Add horizontal grid lines (after points if grid=="over")
  if ( gridPos == "over" ) {
    graphics::abline(h = graphics::axTicks(2)[-1], col=gridCol, lwd=gridLwd, lty=gridLty)
  }
  
  # Y axis
  graphics::axis(2, las=1)
  
  # Nicely formatted time axis
  if ( !localTime ) {
    at1 <- time[seq(1,length(time),6)]
    labels1 <- strftime(at1, "%H",  tz = "UTC")
    graphics::axis.POSIXct(1, time, at=at1, labels=labels1, tcl = -0.25, mgp = c(1,0.5,0))
  } else {
    at1 <- time[seq(1,length(time),6)]
    labels1 <- strftime(at1, "%H")
    graphics::axis.POSIXct(1, time, at=at1, labels=labels1, tcl = -0.25, mgp = c(1,0.5,0))
  }
  
  # Additional tickless axis for locating noons on the time axis and label them as the Dayname, Month Day
  noons <- time[lubridate::hour(time) == 12]
  at2 <- noons
  
  if ( !localTime ) {
    labels2 <- strftime(at2, "%a, %b %d UTC")
    graphics::axis.POSIXct(1, time, at=at2, labels=labels2, tick = FALSE, line = 1)
  } else {
    labels2 <- strftime(at2, "%a, %b %d %Z")
    graphics::axis.POSIXct(1, time, at=at2, labels=labels2, tick = FALSE, line = 1)
  }
  
  if ( shadedNight ) {
    timeInfo <- PWFSLSmoke::timeInfo(bs_grid$time, targetLon, targetLat)
    PWFSLSmoke::addShadedNight(timeInfo)
  }
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaSpatialUtils)
  library(AirFireModeling)
  setModelDataDir("~/Data/Bluesky")
  
  bs_grid <- bluesky_load(model = "PNW-1.33km", modelRun = 2019100900)
  tlim <- NULL
  localTime <- FALSE
  shadedNight <- T
  add <- FALSE
  gridPos <- ""
  gridCol <- "black"
  gridLwd <- 1
  gridLty <- "solid"
  dayLwd <- 0
  hourLwd <- 0
  hourInterval <- 6
  title <- NULL
  ylim <- c(0, 500)
  
}

