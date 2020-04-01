#' #' @export
#' #' @title Subset a Raster* by distance from a location
#' #'
#' #' @param raster A Raster* object.
#' #' @param longitude Target longitude from which the radius will be calculated.
#' #' @param latitude Target latitude from which the radius will be calculated.
#' #' @param radius Distance (km) of radius from target location.
#' #' @param count Number of grid cells to return.
#' #'
#' #' @param ... Subsetting arguments
#' #'
#' #' @return a Raster* object
#' #'
#' #' @examples
#' #' \dontrun{
#' #' model <- raster_load()
#' #' pnw_4km_subbed <- raster_subset(model, n = 200, longitude = -120, latitude = 45)
#' #'
#' #' library(AirFireModeling)
#' #' setModelDataDir('~/Data/BlueSky')
#' #'
#' #' # Load from server
#' #' rasterList <- raster_load(
#' #'   model = "PNW-4km",
#' #'   modelRun = c(2019100800, 2019100900, 2019101000, 2019101100),
#' #'   xlim = c(-125, -115),
#' #'   ylim = c(42, 50)
#' #' )
#' #'
#' #' Portland_lon <- -122.68
#' #' Portland_lat <- 45.52
#' #'
#' #' abc <- raster_subset(
#' #'   rasterList[[1]],
#' #'   n = 200,
#' #'   longitude <- Portland_lon,
#' #'   latitude <- Portland_lat
#' #' )
#' #'
#' #' }
#' raster_subsetByPolygon <- function(
#'   raster,
#'   ...
#' ) {
#'
#'   # Catch params
#'   args <- list(...)
#'   # NOTE: Sub-Internal subset function to parse raster with args.
#'   .subset <- function(raster, args ) {
#'     if ( 'radius' %in% names(args) ) {
#'       if ( 'longitude' %in% names(args) && 'latitude' %in% names(args) ) {
#'         # Convert raster object to data frame
#'         df <- suppressWarnings(raster::rasterToPoints(raster))
#'         r <- args[['radius']]
#'         lat <- args[['latitude']]
#'         lon <- args[['longitude']]
#'         # Calculate distances from target latitiude and longitude
#'         target_dist <- geosphere::distGeo(p1 = c(lon, lat), p2 = df[,1:2])
#'         # Subset around coordinates in given radius
#'         in_radius <- df[which(target_dist <= r),]
#'         # Convert subsetted df back to raster
#'         ras <- suppressWarnings(
#'           raster::rasterFromXYZ(in_radius)
#'         )
#'         return(ras)
#'       } else {
#'         stop('Radial subsetting requires a target coordinates - latitude, longitude.')
#'       }
#'     } else if ( 'polygon' %in% names(args) ) {
#'       p <- args[['polygon']]
#'       # Mask using spatial polygon
#'       ras <- suppressWarnings(
#'         raster::mask(raster, p)
#'       )
#'       return(ras)
#'     } else if ( 'n' %in% names(args) ) { # Adjacent cell count
#'       n <- args[['n']]
#'       lat <- args[['latitude']]
#'       lon <- args[['longitude']]
#'       snapToGrid <- args[['snapToGrid']]
#'       # Determine if snapToGrid is true
#'       s2g <- ifelse(is.logical(snapToGrid), snapToGrid, FALSE)
#'       if ( s2g ) {
#'         target_cell <- raster::cellFromXY(raster, c(lon, lat))
#'         expand <- function(n) {
#'           M <- matrix(1, ncol = n*2+1, nrow = n*2+1)
#'           M[n+1,n+1] <- 0
#'           return(M)
#'         }
#'         adj <- raster::adjacent( raster,
#'                                  cells = target_cell,
#'                                  direction = expand(n),
#'                                  include = TRUE )[,2]
#'         ras <- raster::crop(raster, raster::extentFromCells(raster, cells = adj))
#'         return(ras)
#'       } else {
#'         # NOTE: Hacky way of solving raster even # cell count issue
#'         # Get cell distances from target
#'         coords <- raster::coordinates(raster)
#'         dist <- geosphere::distHaversine(c(lon, lat), coords)
#'         # Order the coords based on dist, select N
#'         xy <- coords[order(dist),][1:n,]
#'         cell <- raster::cellFromXY(raster, xy)
#'         # Get timeseries from relevant cells
#'         z <- raster[raster::rowColFromCell(raster, cell)]
#'         # Create raster from coords and timeseries
#'         ras <- raster::rasterFromXYZ(data.frame(xy, z), crs = raster::crs(raster))
#'         return(ras)
#'       }
#'     } else {
#'       stop('Missing subset arguments.')
#'     }
#'   }
#'   cl <- parallel::makeCluster(future::availableCores() - 1)
#'   future::plan(strategy = future::cluster, workers = cl)
#'   if ( class(raster) == 'list' ) {
#'     models <- future.apply::future_lapply(
#'       X = raster,
#'       FUN = function(r) {
#'         .subset(r, args)
#'       }
#'     )
#'   } else if ( stringr::str_detect(class(raster), 'Raster*') ) {
#'     models <- .subset(raster, args)
#'   } else {
#'     stop('Invalid raster object.')
#'   }
#'   parallel::stopCluster(cl)
#'   return(models)
#' }
