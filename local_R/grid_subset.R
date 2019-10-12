#' @keywords bs_grid
#' @export
#' @title Subset \code{bs_grid} Data List to a Smaller Grid
#' @param bs_grid bs_grid data list
#' @param xlim optional longitude range with lo and hi longitude values
#' @param ylim optional latitude range with lo and hi longitude values
#' @param tlim optional time range with lo and hi time values (POSIXct)
#' @description The incoming gridded data list is filtered according to the parameters
#' passed in.  If any parameter is not specified, that parameter will not be used in the filtering.
#' @return bluesky data list with reduced dimensions
#' @examples \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bs <- bluesky_load(model="PNW-1.33km", modelRun=2016091000)
#' xlim <- c(-118, -114)
#' ylim <- c(45, 48)
#' gridMap(bs, xlim=xlim, ylim=ylim, tMask=(1:24))
#' }
#' @seealso \link{grid_subsetByDistance}

grid_subset <- function(bs_grid, xlim=NULL, ylim=NULL, tlim=NULL) {
  
  # Sanity check
  if ( !("bs_grid" %in% class(bs_grid)) ) {
    stop("bs_grid object is not of class 'bs_grid'.")
  }
  
  # TODO:  Add support for 4D grids
  if ( !(length(dim(bs_grid$data[[1]])) == 3) ) {
    stop("Currently, only 3-D grids are supported.")
  }
  
  # Create masks based on lat, lon and time ranges
  if ( is.null(xlim) ) {    
    lonMask <- rep(TRUE,length(bs_grid$longitude))
  } else {
    lonMask <- bs_grid$longitude >= xlim[1] & bs_grid$longitude <= xlim[2]
  }
  
  if ( is.null(ylim) ) {
    latMask <- rep(TRUE,length(bs_grid$latitude))
  } else {
    latMask <- bs_grid$latitude >= ylim[1] & bs_grid$latitude <= ylim[2]
  }
  
  if ( is.null(tlim) ) {
    tMask <- rep(TRUE,length(bs_grid$time))
  } else {
    tMask <- bs_grid$time >= tlim[1] & bs_grid$time <= tlim[2]
  }
  
  # Subset using masks
  bs_grid$longitude <- bs_grid$longitude[lonMask]
  bs_grid$latitude <- bs_grid$latitude[latMask]
  bs_grid$time <- bs_grid$time[tMask]
  for (param in names(bs_grid$data)) {
    bs_grid$data[[param]] <- bs_grid$data[[param]][lonMask, latMask, tMask]
  }
  
  return(bs_grid)
  
}
