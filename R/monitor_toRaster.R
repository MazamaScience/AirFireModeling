monitor_toRaster <- function(
  ws_monitor,
  res,
  crs = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0',
  extent = NULL,
  from = NULL
) {

  roundCell <- function(x, r) ceiling(x*(1/r))/(1/r)
  M <- data.frame( x = roundCell(ws_monitor$meta$longitude, res),
                   y = roundCell(ws_monitor$meta$latitude, res),
                   z = t(ws_monitor$data)[-1,] )
  ras <- raster::rasterFromXYZ(M)

  #tm <- strftime(ws_monitor$data$datetime, '%Y-%m-%d %H:%M', tz = 'UTC')
  #names(ras) <- tm
  if ( !is.null(crs) ) raster::crs(ras) <- crs
  if ( !is.null(extent) ) raster::extent(ras) <- extent

  if ( !is.null(from) ) {
    ras <- raster::projectRaster(from = ras, to = from)
  }
  return(ras)
}
