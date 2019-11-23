raster_correlation <-
  function(x, y, method = 'kendall', p_value = FALSE) {
    raster::corLocal(x,y,method = method, test = p_value)
  }
