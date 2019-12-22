#' Title
#'
#' @param raster
#' @param longitude
#' @param latitude
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
bluesky_spaghetti <- function( raster,
                               longitude = NULL,
                               latitude = NULL,
                               radius = 5000 ) {

  # Subset the raster to radius
  subbed <- raster_subset(raster, longitude, latitude, radius)

  raster::crs(subbed) <- raster::crs(raster)

  # Use each coordiante cell -> convert to monitor -> combine mons -> plot
  coords <- raster::coordinates(subbed)

  unique_names <- mapply(
    function(x,y) {
      paste('monitor', x, y, sep = '_')
    },
    coords[,1],
    coords[,2]
  )

  monitor_list <- mapply(
    function(x,y,n) {
      raster_toMonitor(raster, x, y, monitorID = n)
    },
    coords[,1],
    coords[,2],
    unique_names,
    SIMPLIFY = FALSE
  )

  monitors <- PWFSLSmoke::monitor_combine(monitor_list)

}
