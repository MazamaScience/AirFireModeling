#' @export
#' @title Create a Raster from monitor object
#'
#' @param ws_monitor \pkg{PWFSLSMoke} \emph{ws_monitor} object.
#' @param raster Raster* object defining the output grid.
#' @param FUN Function used to collapse values when multiple monitors lie within
#' a single grid cell.
#'
#' @return A \code{RasterBrick} object.
#'
monitor_toRaster <- function(
  ws_monitor = NULL,
  raster = NULL,
  FUN = mean
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(ws_monitor)
  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(FUN)

  if ( !PWFSLSmoke::monitor_isMonitor(ws_monitor) )
    stop("Parameter 'ws_monitor' is not a valid ws_monitor object.")

  if ( PWFSLSmoke::monitor_isEmpty(ws_monitor) )
    stop("Parameter 'ws_monitor' contains no data.")

  if ( !raster_isRaster(raster) )
    stop("Parameter 'raster' must be a single Raster* object.")

  # # ---- Get all of the monitor locations ----------------------------------------
  # us_monitors <- monitor_loadAnnual(2018) %>% monitor_subset(tlim = c(20180601,20181030))
  #
  # # ---- Load US States and plot the monitor locations over it. ------------------
  # loadSpatialData("USCensusStates")
  #
  # # create a CONUS subset that excludes Alaska, Hawaii, etc.
  # omit_codes <- c("HI", "AK", "AS", "PR", "GU", "MP", "VI")
  # conus_states <- subset(USCensusStates, !(USCensusStates@data$stateCode %in% omit_codes))
  # plot(conus_states)
  #
  # # Get the monitor locations into a SpatialPointsDataFrame. This will allow us to
  # # use rgeos and do some spatial subsetting
  #
  # monitor_location_df <- subset(us_monitors$meta,
  #                               select = c("monitorID", "longitude", "latitude", "stateCode", "countryCode"))
  #
  # xy <- monitor_location_df[c("longitude", "latitude")]
  #
  # monitor_spdf <- SpatialPointsDataFrame(coords = xy, data = monitor_location_df,
  #                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))






  # # Checks
  # if ( ncol(ws_monitor$data) == 2 ) {
  #   stop('Needs more than a single ws_monitor object.')
  # }
  # if ( PWFSLSmoke::monitor_isEmpty(ws_monitor) ) {
  #   stop('Cannot be an empty ws_monitor object.')
  # }
  #
  # # Round the coordinates to the cell resolution
  # roundCell <- function(x, r) ceiling(x*(1/r))/(1/r)
  #
  # # Create the XYZ data.frame with x lat, y lon, and z data
  # M <- data.frame( x = roundCell(ws_monitor$meta$longitude, res),
  #                  y = roundCell(ws_monitor$meta$latitude, res),
  #                  z = t(ws_monitor$data[,-1]) )
  # # Create raster from coordinates and values
  # ras <-
  #   suppressWarnings({ raster::rasterFromXYZ( xyz = M,
  #                                             crs = crs,
  #                                             res = res ) })
  #
  # # Apply POSIX numeric names to raster layers
  # names(ras) <- as.numeric(ws_monitor$data$datetime)
  #
  # # Project to a different raster crs
  # if ( !is.null(projectTo) ) {
  #   ras <-
  #     suppressWarnings({ raster::projectRaster( from = ras,
  #                                               to = projectTo,
  #                                               method = 'bilinear' ) })
  # }
  #
  # return(ras)

}
