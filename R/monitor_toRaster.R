#' @export
#' @title Create a Raster from monitor object
#'
#' @param ws_monitor A ws_monitor object.
#' @param res Set the resolution of a Raster* object
#' @param crs Set the coordinate reference system (CRS) of a Raster* object.
#' @param extent Set or inherit the extent of the Raster* object.
#' @param projectTo
#'
#' @return
monitor_toRaster <- function(
  ws_monitor,
  res,
  crs = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0',
  extent = NULL,
  projectTo = NULL
) {

  # Checks
  if ( ncol(ws_monitor$data) == 2 ) {
    stop('Needs more than a single ws_monitor object.')
  }
  if ( PWFSLSmoke::monitor_isEmpty(ws_monitor) ) {
    stop('Cannot be an empty ws_monitor object.')
  }

  # Round the cells
  roundCell <- function(x, r) ceiling(x*(1/r))/(1/r)

  # Create the XYZ data.frame with x lat, y lon, and z data
  M <- data.frame( x = roundCell(ws_monitor$meta$longitude, res),
                   y = roundCell(ws_monitor$meta$latitude, res),
                   z = t(ws_monitor$data[,-1]) )
  # Create raster
  ras <- raster::rasterFromXYZ(xyz = M, crs = crs,res = res )

  # Apply POSIX numeric names to raster layers
  names(ras) <- as.numeric(ws_monitor$data$datetime)

  # if ( !is.null(crs) ) raster::crs(ras) <- crs
  # if ( !is.null(extent) ) raster::extent(ras) <- extent

  if ( !is.null(projectTo) ) {
    ras <- raster::projectRaster(from = ras, to = projectTo)
  }
  return(ras)

  if (FALSE) {
    ws_monitor <- yuba
    res <- 0.5
    crs <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
    projectTo <- bs_raster
  }

}
