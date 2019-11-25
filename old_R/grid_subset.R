#' @export
#' 
#' @title Subset \code{bs_grid} to create a smaller grid
#' 
#' @param bs_grid \emph{bs_grid} data list.
#' @param xlim Optional vector of longitude lo and hi values.
#' @param ylim Optional vector of latitude lo and hi values.
#' @param tlim Optional vector of time lo and hi values in Ymd[HMS] format or
#' \code{POSIXct}.
#' 
#' @description The incoming gridded data list is subset according to the 
#' parameters passed in.
#' 
#' @return \code{bs_grid} object with reduced dimensions.
#' 
#' @examples \donttest{
#' setModelDataDir('~/Data/Bluesky')
#' bs <- bluesky_load(model = "PNW-1.33km", modelRun = 2019100900)
#' xlim <- c(-118, -114)
#' ylim <- c(45, 48)
#' grid_map(bs, xlim = xlim, ylim = ylim, tMask = (1:24))
#' }

grid_subset <- function(
  bs_grid,
  xlim = NULL, 
  ylim = NULL, 
  tlim = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Sanity check
  if ( !grid_isGrid(bs_grid) ) {
    stop("Parameter 'bs_grid' is not a proper 'bs_grid' object")
  }
  
  # TODO:  Add support for 4D grids
  if ( !(length(dim(bs_grid$data[[1]])) == 3) ) {
    stop("Currently, only 3-D grids are supported.")
  }
  
  # ----- Create axis masks ----------------------------------------------------
  
  if ( is.null(xlim) ) {    
    lonMask <- rep(TRUE, length(bs_grid$longitude))
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
  
  # ----- Subset using masks ---------------------------------------------------
  
  bs_grid$longitude <- bs_grid$longitude[lonMask]
  bs_grid$latitude <- bs_grid$latitude[latMask]
  bs_grid$time <- bs_grid$time[tMask]
  
  for (param in names(bs_grid$data)) {
    bs_grid$data[[param]] <- bs_grid$data[[param]][lonMask, latMask, tMask]
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(bs_grid)
  
}
