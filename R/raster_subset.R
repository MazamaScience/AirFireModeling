#' @export
#' @title Subset a Raster* by distance from a location
#'
#' @param raster A Raster* object.
#' @param longitude A target longitude.
#' @param latitude A target latitude.
#' @param radius A radius to subset by (in meters)
#'
#' @return a Raster* object
raster_subset <- function(
  raster,
  longitude,
  latitude,
  radius = 20000
) {

  # Checks
  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)

  # Convert raster object to data frame
  df <- suppressWarnings(
    raster::rasterToPoints(raster)
  )

  # Calculate distances from target latitiude and longitude
  target_dist <- geosphere::distGeo(p1 = c(longitude, latitude), p2 = df[,1:2])

  # Subset around coordinates in given radius
  in_radius <- df[which(target_dist <= radius),]

  # Convert subsetted df back to raster
  ras <- suppressWarnings(
    raster::rasterFromXYZ(in_radius)
  )

  return(ras)

}
