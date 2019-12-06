#' @export
#' @title Subset a Raster* by distance from a location
#'
#' @param raster A Raster* object.
#' @param ... Subsetting arguments
#'
#' @return a Raster* object
#'
#' @examples
#' \dontrun{
#' # Load Bluesky
#' bs <- bluesky_load()
#'
#' # Subset by radius
#' raster_subset(bs, radius = 20000, latitude = 45.5, longitude = -123.32)
#'
#' # Subset with polygon
#' # Create CA polygon
#' ca <- maps::map('state', 'CA', fill = TRUE)
#' ca_polygon <- maptools::map2SpatialPolygons(ca, IDs = ca$names, proj4string = raster::crs(bs))
#'
#' # Raster subset
#' raster_subset(bs, polygon = ca_polygon)
#' }
raster_subset <- function(
  raster,
  ...
) {

  args <- list(...)

  if ( 'radius' %in% names(args) ) {
    if ( 'longitude' %in% names(args) && 'latitude' %in% names(args) ) {

      # Convert raster object to data frame
      df <- suppressWarnings(
        raster::rasterToPoints(raster)
      )
      r <- args[['radius']]
      lat <- args[['latitude']]
      lon <- args[['longitude']]
      # Calculate distances from target latitiude and longitude
      target_dist <- geosphere::distGeo(p1 = c(lon, lat), p2 = df[,1:2])
      # Subset around coordinates in given radius
      in_radius <- df[which(target_dist <= r),]
      # Convert subsetted df back to raster
      ras <- suppressWarnings(
        raster::rasterFromXYZ(in_radius)
      )

      return(ras)

    } else {
      stop('Radial subsetting requires a target coordinates - latitude, longitude.')
    }

  } else if ( 'polygon' %in% names(args) ) {

    p <- args[['polygon']]
    # Mask using spatial polygon
    ras <- suppressWarnings(
      raster::mask(raster, p)
    )

    return(ras)

  } else {
    stop('Missing subset arguments.')
  }

  if ( FALSE ) {
    raster <- bluesky_load()
    args <- list('latitude' = 45, 'longitude' = 118, 'radius' = 20000)
    raster_subset(raster, radius = 20000, latitude = 38, longitude = -118)
    raster_subset(raster, polygon = p) %>% raster::animate()
  }

}
