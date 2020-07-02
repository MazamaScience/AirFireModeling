#' @export
#' @title Subset a Raster* by distance from a location
#'
#' @param raster A Raster* object.
#' @param longitude Target longitude from which the radius will be calculated.
#' @param latitude Target latitude from which the radius will be calculated.
#' @param radius Distance (km) of radius from target location.
#' @param count Number of grid cells within radius to return.
#'
#' @description This function subsets a raster object just like a round cookie
#' cutter. All cells (up to \code{count}) within \code{radius} km of
#' the target location are returned.
#'
#' @return A Raster* object.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   model = "PNW-4km",
#'   modelRun = c(2019100800, 2019100900, 2019101000, 2019101100),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' Portland_area <- raster_subsetByDistance(
#'   rasterList,
#'   longitude = -122.68,
#'   latitude = 45.52,
#'   radius = 100
#' )
#'
#' raster_ggmap(Portland_area)
#' }
raster_subsetByDistance <- function(
  raster = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = 50,
  count = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)

  if ( !is.list(raster) && !raster_isRaster(raster) )
    stop("Parameter 'raster' must be a single or a list of Raster* objects.")

  if ( !is.numeric(longitude) )
    stop("Parameter 'longitude' must be numeric.")

  if ( !is.numeric(latitude) )
    stop("Parameter 'latitude' must be numeric.")

  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be numeric.")

  if ( !is.null(count) ) {
    if ( !is.numeric(count) )
      stop("Parameter 'count' must be numeric.")
  }

  # ----- Subset the Raster(s) -------------------------------------------------

  if ( raster_isRaster(raster) ) {

    rasterBrick <- .subsetByDistance(
      raster = raster,
      longitude = longitude,
      latitude = latitude,
      radius = radius,
      count = count
    )

    return(rasterBrick)

  } else {

    rasterList <- lapply(
      X = raster,
      FUN = function(r) {
        .subsetByDistance(
          r,
          longitude = longitude,
          latitude = latitude,
          radius = radius,
          count = count
        )
      }
    )

    return(rasterList)

  }

}

# ===== Internal Functions =====================================================

# @keywords internal
# @title Internal subsetByDistance function
#
# @param raster A Raster* object.
# @param longitude Target longitude from which the radius will be calculated.
# @param latitude Target latitude from which the radius will be calculated.
# @param radius Distance (km) of radius from target location.
# @param count Number of grid cells within radius to return.
#
.subsetByDistance <- function(
  raster,
  longitude = NULL,
  latitude = NULL,
  radius = 50,
  count = NULL
) {

  # All parameters validated in the calling function

  # Convert raster object to rasterPoints matrix
  pointsMatrix <- suppressWarnings(raster::rasterToPoints(raster))

  # Calculate distances (in km) from target latitiude and longitude
  distanceVector <- geosphere::distGeo(p1 = c(longitude, latitude),
                                       p2 = pointsMatrix[,1:2]) / 1000

  # Subset by radius
  closestDistances <- distanceVector[distanceVector < radius]

  # Or subset by count if we need fewer
  if ( !is.null(count) && count < length(closestDistances) ) {
    closestIndices <- order(distanceVector)[1:count]
    closestDistances <- distanceVector[closestIndices]
  }

  # Subset pointsMatrix
  nearbyPoints <- pointsMatrix[which(distanceVector <= max(closestDistances)),]

  # Convert subset pointsMatrix back to raster
  rasterBrick <- suppressWarnings(
    raster::rasterFromXYZ(nearbyPoints)
  )

  # Restore original projection information
  raster::crs(rasterBrick) <- raster::crs(raster)

  return(rasterBrick)

}
