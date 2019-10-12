#' @export
#' 
#' @title Create distance-from-targetgGrid
#' 
#' @param bs_grid \emph{bs_grid} object.
#' @param longitude Longitude of the point from which distances are calculated
#' in decimal degrees E.
#' @param latitude Latitude of the point from which distances are calculated
#' in decimal degrees N.
#' 
#' @description This function returns 2D matrix (aka 'grid'). Each cell in the 
#' grid contains the distance in meters between the center of that cell and the 
#' target location. This distance grid can be used to create a mask identifying 
#' grid cells within a certain radius of the target location.
#' 
#' @return 2d matrix of distances.
#' 
#' @examples
#' \donttest{
#' setModelDataDir("~/Data/Bluesky")
#' bs <- bluesky_load(model = "PNW-4km", modelRun = 2019090900)
#' xlim <- c(-124, -122)
#' ylim <- c(46, 48)
#' bs <- grid_subset(bs, xlim=xlim, ylim=ylim)
#' distanceMatrix <- grid_distance(bs, -123.801, 47.704)
#' image(bs$longitude, bs$latitude, distanceMatrix, xlab = "", ylab = "")
#' }

grid_distance <- function(
  bs_grid = NULL, 
  longitude, 
  latitude
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(bs_grid)

  if ( !("bs_grid" %in% class(bs_grid)) )
    stop("bs_grid object is not of class 'bs_grid'.")
  
  # ----- Calculate distances --------------------------------------------------
  
  allLons <- rep(bs_grid$longitude, times = length(bs_grid$latitude))
  allLats <- rep(bs_grid$latitude, each = length(bs_grid$longitude))
  
  # Use the incredibly fast geodist package
  allDistances <-
    geodist::geodist(
      x = cbind(
        "x" = longitude,
        "y" = latitude
      ),
      y = cbind(
        "x" = allLons,
        "y" = allLats
      )
    )
  
  # At this point, allDistances is a matrix with 1 row and N columns
  allDistances <- allDistances[1,]
  
  # Create 2D gridded version of distance
  distanceGrid <- matrix(
    allDistances, 
    nrow = length(bs_grid$longitude), 
    ncol = length(bs_grid$latitude), 
    byrow = FALSE
  )
  
  # NOTE:  Test the result visually with: 
  # NOTE:    image(bs_grid$longitude, bs_grid$latitude, distanceGrid)
  
  return(distanceGrid)
  
}
