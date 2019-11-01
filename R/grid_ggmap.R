#' @export
#'
#' @title Map gridded model data with ggplot2
#'
#' @param bs_grid \emph{bs_grid} object
#' @param parameter Data parameter to plot.
#' @param xlim Longitude range to be plotted.
#' @param ylim Latitude range to be plotted.
#' @param tMask Logical vector of the same length as the time axis or a set
#' of indices used to select specific times.
#' @param slice Either a time index or a function used to collapse the time axis.
#'
#' @description Use the \pkg{ggplot2} package to plot a map of bluesky data in a
#' given area.
#'
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bs_grid <- bluesky_load(model = "PNW-1.33km", modelRun = 2019100900, subDir = 'forecast')
#' grid_ggmap(bs_grid, xlim = xlim, ylim = ylim, tMask = 1:40, slice = max)
#' }
grid_ggmap <- function(
  bs_grid,
  parameter = "pm25",
  xlim = range(bs_grid$longitude),
  ylim = range(bs_grid$latitude),
  tMask = rep(TRUE, times = length(bs_grid$time)),
  slice = mean
) {
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(bs_grid)
  MazamaCoreUtils::stopIfNull(parameter)
  MazamaCoreUtils::stopIfNull(xlim)
  MazamaCoreUtils::stopIfNull(ylim)
  MazamaCoreUtils::stopIfNull(tMask)
  MazamaCoreUtils::stopIfNull(slice)

  if ( !("bs_grid" %in% class(bs_grid)) )
    stop("bs_grid object is not of class 'bs_grid'.")

  if ( !(length(dim(bs_grid$data[[1]])) == 3) )
    stop("Currently, only 3-D grids are supported.")

  if ( !is.logical(addAxes) )
    addAxes <- TRUE

  # maps package doesn't include states outside of CONUS
  if ( xlim[1] > -65 ||
       xlim[2] < -125 ||
       ylim[1] > 50 ||
       ylim[2] < 25)
    stop("grid_map() can only create plots within the continental US")

  # ----- Subset the grid ------------------------------------------------------

  bs_gridSub <- grid_subset(bs_grid, xlim, ylim)
  lon <- bs_gridSub$longitude
  lat <- bs_gridSub$latitude
  dataGrid <- bs_gridSub$data[[parameter]][,,tMask]
  timeRange <- range(bs_grid$time[tMask])

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

  # ----- ggplot ---------------------------------------------------------------
  states <- ggplot2::map_data('state', xlim = xlim, ylim = ylim)
  counties <- ggplot2::map_data('county', xlim = xlim, ylim = ylim)

  gs <- t(gridSlice)[nrow(t(gridSlice)):1,] # flip coords

  ras <- raster::raster(gs, xmn = xlim[1], xmx = xlim[2], ymn = ylim[1], ymx = ylim[2])

  gg <- rasterVis::gplot(ras) +
    ggplot2::geom_tile(ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value)) +
    ggplot2::geom_path(data = counties, ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group), alpha = 0.2, color = 'grey12') +
    ggplot2::geom_polygon(data = states, ggplot2::aes(y = .data$lat, x = .data$long, group = .data$group), fill = 'NA', color = 'black') +
    ggplot2::scale_fill_gradient(na.value = 'NA', low = 'white', high = 'black') +
    ggplot2::labs(title = bs_grid$model,
                  subtitle = paste0(timeRange[1], " to ",timeRange[2]),
                  x = 'Longitude', y = 'Latitude', fill = 'Model PM2.5') +
    ggplot2::theme_classic() +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim, ratio = 4/3)

  return(gg)

# ===== DEBUGGING =============================================================

if ( FALSE ) {

  ###https://tools.airfire.org/websky/v1/run/standard/CANSAC-4km/2019101100

  setModelDataDir('~/Data/Bluesky')
  bs <- bluesky_load(model = "CANSAC-4km",
                     modelRun = 2019101100,
                     subDir = "forecast")
  xlim <- c(-124, -118)
  ylim <- c(35, 41)
  grid_ggmap(bs, xlim = xlim, ylim = ylim, slice = mean)

}
}
