#' @export
#' @title Local correlation coefficient
#'
#' @param x A Raster* object
#' @param y A Raster* object
#' @param res An optional resolution to project both x, y to.
#' @param method Correlation method. Default: 'kendall'.
#' @param p_value Logical. If TRUE, return a p-value
#' @param crs Set the coordinate reference system.
#'
#' @return a Raster* object
raster_correlation <-
  function(
    x,
    y,
    res = NULL,
    method = 'kendall',
    p_value = FALSE,
    crs = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
  ) {

    MazamaCoreUtils::stopIfNull(x)
    MazamaCoreUtils::stopIfNull(y)
    MazamaCoreUtils::stopIfNull(method)
    MazamaCoreUtils::stopIfNull(p_value)

    # if ( raster::isLonLat(x) | raster::isLonLat(y) ) {
    #   stop('Raster needs latitude/longitutde coordinates.')
    # }

    # Transform to same length
    # If resolution is unspecified use the largest resolution by default
    if ( is.null(res) ) {
      res <-
        ifelse( raster::res(x)[1] > raster::res(y)[1],
                raster::res(x),
                raster::res(y) )
    }

    # Tranform rasters to matching resolution projections
    x <- raster::projectRaster(x, res = res, crs = crs)
    y <- raster::projectRaster(y, res = res, crs = crs)

    # Calculate approx area of each raster to determine which to project from -> to
    x_ext <- raster::extent(x)
    y_ext <- raster::extent(y)
    x_area <- (x_ext@xmax - x_ext@xmin)*(x_ext@ymax - x_ext@ymin)
    y_area <- (y_ext@xmax - y_ext@xmin)*(y_ext@ymax - y_ext@ymin)

    # Determine which raster surf area is larger, use the larger one
    if ( x_area < y_area ) {
      x <- raster::projectRaster(from = x, to = y)
    } else {
      y <- raster::projectRaster(from = y, to = x)
    }

    # Truncate layers to whichever rasterbrick has less layers\
    x_layers <- raster::nlayers(x)
    y_layers <- raster::nlayers(y)

    if ( x_layers != y_layers ) {
      layers <- ifelse(x_layers > y_layers, y_layers, x_layers)
    } else {
      layers <- x_layers
    }

    # Calculate correlation
    correlation <- raster::corLocal( x[[1:layers]],
                                     y[[1:layers]],
                                     method = method,
                                     test = p_value )
    # TODO: Implement mapping logical, hide/resolve warnings
    return(correlation)

    if ( FALSE ) {
      x <- ca_raster
      y <- bs_raster
      res <- 0.5
      method  <- 'kendall'
      p_value <- F
      crs <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
    }

  }
