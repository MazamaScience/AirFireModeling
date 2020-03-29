#' @export
#'
#' @title Collapse a \emph{bs_grid} object into a \emph{ws_monitor} object with
#' a single monitor
#'
#' @param bs_grid \emph{bs_grid} object.
#' @param longitude Target longitude passed on to \code{grid_subsetByDistance()}.
#' @param latitude Target latitude passed on to \code{grid_subsetByDistance()}.
#' @param monitorID Assigned monitorID of the created monitor.
#' (Default = "generated_id")
#' @param radius Radius (m) passed on to \code{grid_subsetByDistance()}.
#' @param count Radius (m) passed on to \code{grid_subsetByDistance()}.
#' @param FUN The function used to collapse all data at a single time index.
#' (Default = mean)
#' @param ... Any additional arguments used by \code{FUN}, \emph{e.g.}
#' \code{na.rm = TRUE}
#'
#' @description This function combines the operations of
#' \code{grid_subsetByDistance()} and \code{PWFSLSmoke::monitor_collapse()}.
#' Time series associated with multiple grid cells are merged into a single
#' time series by using \code{FUN} to collapse a given grid cell count, or
#' or a grid cell radi, to a single coordinate. For instance, if the
#' \code{FUN = mean} then the grid cells within the paramters are averaged to a
#' single central coordinate.
#' Any function acceptable to \code{PWFSLSmoke::monitor_collapse()} can be used.
#'
#' @return A \emph{ws_monitor} object representing a single monitor.
#'
#' @examples
#' \dontrun{
#' # Next three lines required for bs_grid to ws_monitor conversion
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#'
#' # Now we can work as we normally would
#' setModelDataDir("~/Data/Bluesky")
#' bs <- bluesky_load(
#'   model = "CANSAC-1.33km",
#'   modelRun = 2019100900,
#'   subDir = "forecast"
#' )
#'
#' # Get the Yosmite monitor
#' Yosemite_Village <-
#'   monitor_load(20191009, 20191012) %>%
#'   monitor_subset(monitorIDs = "060431001_01")
#'
#' # Create a fake version using the 80'th percentile
#' Yosemite_fake <- grid_createMonitor(
#'   bs,
#'   longitude = Yosemite_Village$meta$longitude,
#'   latitude = Yosemite_Village$meta$latitude,
#'   radius = 10000,
#'   monitorID = "Model data",
#'   FUN = quantile,
#'   probs = 0.90,
#'   na.rm = TRUE
#' )
#'
#' # Combine them and use AirMonitorPlots to plot them
#' Yosemite_combined <-
#'   PWFSLSmoke::monitor_combine(list(Yosemite_Village, Yosemite_fake))
#'
#' library(AirMonitorPlots)
#'
#' ggplot_pm25Timeseries(Yosemite_combined) +
#'   geom_pm25Points(aes(color = monitorID)) +
#'   stat_nowcast(aes(color = monitorID)) +
#'   ggtitle("CANSAC-1.33km vs. Monitoring Data")
#' }

grid_createMonitor <- function(
  bs_grid,
  longitude = NULL,
  latitude = NULL,
  radius = 20000,
  count = NULL,
  monitorID = "generated_id",
  FUN = mean,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(bs_grid)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)

  if ( !("bs_grid" %in% class(bs_grid)) )
    stop("bs_grid object is not of class 'bs_grid'.")

  if ( ! length(dim(bs_grid$data[[1]])) == 3 ) {
    stop("Currently, only 3-D grids are supported.")
  }

  # ----- Subset by distance ---------------------------------------------------

  if ( is.null(count) ) {
    ws_monitor <- grid_subsetByDistance(bs_grid, longitude, latitude, radius)
  } else {
    ws_monitor <- grid_subsetByDistance(bs_grid, longitude, latitude, radius, count)
  }

  # ----- Collapse -------------------------------------------------------------

  ws_monitor <- PWFSLSmoke::monitor_collapse(
    ws_monitor,
    longitude = longitude,
    latgitude = latitude,
    monitorID = monitorID,
    FUN = FUN,
    ...
  )

  # ----- Return ---------------------------------------------------------------

  return(ws_monitor)

}

