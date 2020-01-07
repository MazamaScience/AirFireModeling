raster_calculate <- function(raster, FUN) {
  return(raster::calc(raster, FUN))
}
