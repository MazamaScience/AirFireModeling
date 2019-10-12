#' @export
#' 
#' @title Map gridded model data
#' 
#' @param bs_grid bs_grid data list
#' @param param data parameter to plot
#' @param xlim longitude range to be plotted
#' @param ylim latitude range to be plotted
#' @param tmask logical time mask to select specific times
#' @param slice either a time index or a function used to collapse the time axis
#' @param style custom styling, one of \code{'default'}
#' @param breaks used by \code{image()} function to partition values into 
#' different colors
#' @param colors vector of colors to use
#' @param addAxes logical specifying whether to add axes
#' @param main title for the plot
#' @param ... additional arguments passed to maps::map()
#' 
#' @description Use the \pkg{maps} package to plot a map of bluesky data in a 
#' given area.
#' 
#' When a named \code{style} is used, some graphical parameters will be 
#' overridden. Available styles include:
#' 
#' \itemize{
#' \item{\code{default}}{-- hourly values are individually colored by 24-hr AQI levels}
#' }
#'
#' If \code{breaks} and \code{colors} are defined, they will override 
#' \code{style}.
#' 
#' @examples
#' \donttest{
#' setModelDataDir('~/Data/Bluesky')
#' bs <- bluesky_load(model = "PNW-1.33km", modelRun = 2019090900)
#' xlim <- c(-118, -114)
#' ylim <- c(45, 48)
#' grid_map(bs, xlim = xlim, ylim = ylim, tmask = (1:40))
#' }

grid_map <- function(
  bs_grid, 
  param = "pm25",
  xlim = range(bs_grid$longitude), 
  ylim = range(bs_grid$latitude), 
  tmask = rep(TRUE, times = length(bs_grid$time)), 
  slice = max,
  style = "default", 
  breaks = NULL, 
  colors = NULL, 
  addAxes = TRUE, 
  main = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(bs_grid)
  MazamaCoreUtils::stopIfNull(param)
  MazamaCoreUtils::stopIfNull(xlim)
  MazamaCoreUtils::stopIfNull(ylim)
  MazamaCoreUtils::stopIfNull(tmask)
  MazamaCoreUtils::stopIfNull(slice)
  MazamaCoreUtils::stopIfNull(style)
  
  if ( !("bs_grid" %in% class(bs_grid)) ) {
    stop("bs_grid object is not of class 'bs_grid'.")
  }
  
  if ( !(length(dim(bs_grid$data[[1]])) == 3) ) {
    stop("Currently, only 3-D grids are supported.")
  }
  
  if ( !is.logical(addAxes) )
    addAxes <- TRUE
  
  # ----- Subset the grid ------------------------------------------------------
  
  bs_gridSub <- grid_subset(bs_grid, xlim, ylim)
  lon <- bs_gridSub$longitude
  lat <- bs_gridSub$latitude
  dataGrid <- bs_gridSub$data[[param]][,,tmask]
  timeRange <- range(bs_grid$time[tmask])
  
  # specify which map slice to plot, or how to aggregate values across time
  if ( is.null(slice) ) {
    stop("Need to specify a slice")
  } else if ( class(slice) == "function" ) {
    gridSlice <- apply(dataGrid, 1:2, slice)
  } else if ( class(slice) == "integer" || class(slice) == "numeric" ) {
    gridSlice <- dataGrid[,,as.integer(slice)]
  } else {
    stop("Improper use of slice argument")
  }
  
  # ----- Plot options ---------------------------------------------------------
  
  # TODO:  Find better way to deal with colors. Are there newer ones?
  
  # new breaks logic
  if ( is.null(breaks) && is.null(colors) ) {
    if ( style == 'default' ) {
      # NOTE: Colors and breaks are the same as those used at https://www.airfire.org/data/bluesky-daily/
      breaks <- c(1,5,10,20,40,90,140,350,525)
      breaks <- c(min(c(0,gridSlice),na.rm=TRUE),breaks)
      breaks <- c(breaks,max(c(600,gridSlice),na.rm=TRUE))
      red <- c(255,255,255,255,255,255,255,200,150)/255
      green <- c(225,195,165,135,105,75,46,2,3)/255
      blue <- c(225,195,165,135,105,75,45,3,3)/255
      colors <- c('transparent',grDevices::rgb(red=red, green=green, blue=blue))
      # TODO:
      # } else {
      #   if ( all(breaks==AQI$breaks_24) ) {
      #     if ( length(breaks) != (length(colors) + 1) ) {
      #       stop(paste0("Length of breaks [",length(breaks),"] must be one more than length of colors [",length(colors),"]"))
      #     }
      #   }
    }
  }
  
  # ----- Plot the map ---------------------------------------------------------
  
  maps::map('state', xlim = xlim, ylim = ylim, ...)
  
  graphics::image(lon, lat, gridSlice, breaks = breaks, col = colors, add = TRUE)
  
  if ( is.null(main) ) {
    main <- paste0(
      bs_grid$model, "\n", 
      timeRange[1], " to ",timeRange[2], " UTC"
    )
  }
  graphics::title(main = main)
  
  result <- try({
    maps::map('county', col = 'gray80', xlim = xlim, ylim = ylim, add = TRUE)
  }, silent=TRUE )
  
  result <- try({
    maps::map('state', xlim = xlim, ylim = ylim, add = TRUE)
  }, silent=TRUE )
  
  if ( addAxes ) {
    maps::map.axes(las = 1)
  } else {
    graphics::box()
  }
  
  # No return
  
}

# ===== DEBUGGING =============================================================

if ( FALSE ) {
  
  ###https://tools.airfire.org/websky/v1/run/standard/CANSAC-4km/2019101100
  
  setModelDataDir('~/Data/Bluesky')
  bs <- bluesky_load(model = "CANSAC-4km", 
                     modelRun = 2019101100,
                     subDir = "forecast")
  xlim <- c(-124, -118)
  ylim <- c(35, 41)
  grid_map(bs, xlim = xlim, ylim = ylim, slice = mean)
  
}
