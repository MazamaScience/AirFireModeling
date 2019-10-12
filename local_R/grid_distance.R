#' @keywords bs_grid
#' @export
#' @title Create Distance-From-Target Grid
#' @param bs_grid bs_grid data list
#' @param longitude longitude (decimal degrees) of the point from which distances are calculated
#' @param latitude latitude (decimal degrees) of the point from which distances are calculated
#' @description This function returns 2D matrix (aka 'grid'). Each cell in the grid
#' contains the distance (km) between the center of that cell and the target location.
#' This distance grid can be used to create a mask identifying grid cells within a certain
#' radius of the target location.
#' @return 2d matrix of distances.
#' @seealso \link{distance}
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bs <- bluesky_load(model="PNW-4km", modelRun=2016091200)
#' xlim <- c(-124, -122)
#' ylim <- c(46, 48)
#' bs <- grid_subset(bs, xlim=xlim, ylim=ylim)
#' distanceMatrix <- grid_distance(bs, -123.801, 47.704)
#' image(bs$longitude, bs$latitude, distanceMatrix, xlab='', ylab='')
#' }

grid_distance <- function(bs_grid, longitude, latitude) {
  
  # Get the distance at every grid cell location
  allLons <- rep(bs_grid$longitude, times = length(bs_grid$latitude))
  allLats <- rep(bs_grid$latitude, each = length(bs_grid$longitude))
  allDistances <- distance(longitude, latitude, allLons, allLats)
  
  # Create 2D gridded version of distance
  distanceGrid <- matrix(allDistances, nrow=length(bs_grid$longitude), ncol=length(bs_grid$latitude), byrow=FALSE)
  
  # NOTE:  Test the result visually with:  image(bs_grid$longitude,bs_grid$latitude,distanceGrid)
  
  return(distanceGrid)
  
}
