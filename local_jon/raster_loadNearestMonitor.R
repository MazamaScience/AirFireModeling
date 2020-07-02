#' #' Load nearest monitor to coordinate location
#' #'
#' #' @param raster A raster* object
#' #' @param longitude A latitude
#' #' @param latitude A longitude
#' #'
#' #' @return a ws_onitor
#' #' @export
#' raster_loadNearestMonitor <- function(raster, longitude, latitude) {
#'   UseMethod('raster_loadNearestMonitor', raster)
#' }
#' #' @describeIn raster_loadNearestMonitor
#' #' @export
#' raster_loadNearestMonitor.Raster <- function(raster, longitude, latitude) {
#'
#'   if ( abs(longitude) < abs(raster@extent@xmax) ||
#'        abs(longitude) > abs(raster@extent@xmin) ) {
#'     stop('longitudegitude not within domain.')
#'   }
#'   if ( abs(latitude) > abs(raster@extent@ymax) ||
#'        abs(latitude) < abs(raster@extent@ymin) ) {
#'     stop('latitudeitude not within domain.')
#'   }
#'
#'   # Parse dates stored in model
#'   # NOTE: model dates stored in layer name
#'   model_dates <- as.numeric(stringr::str_remove(raster@data@names, pattern = 'X'))
#'   class(model_dates) <- c('POSIXct', 'POSIXt')
#'   attr(model_dates, 'tzone') <- 'UTC'
#'   # Load All monitors for model dates
#'   startdate <- range(model_dates)[1]
#'   enddate <- range(model_dates)[2]
#'   ws_monitor <- PWFSLSmoke::monitor_load(startdate, enddate)
#'
#'
#'   monitors_dist <- geosphere::distHaversine( cbind(ws_monitor$meta$longitude,
#'                                                    ws_monitor$meta$latitude),
#'                                              cbind(longitude, latitude) )
#'   nearest_monitorID <- ws_monitor$meta$monitorID[which.min(monitors_dist)]
#'   target_dist <- monitors_dist[which.min(monitors_dist)]
#'   target_monitor <- PWFSLSmoke::monitor_subset(ws_monitor,
#'                                                monitorIDs = nearest_monitorID)
#'   target_monitor$meta$dist <- target_dist
#'
#'   model_name <- stringr::str_extract(raster@file@name, '(?<=[/])([^/]+)(?=\\_[^.]+)')
#'   model_monitor <- raster_toMonitor(raster, longitude, latitude, monitorID = model_name)
#'   combined <- PWFSLSmoke::monitor_combine(list(model_monitor, target_monitor))
#'   return(combined)
#'
#' }
#' #' @describeIn raster_loadNearestMonitor
#' #' @export
#' raster_loadNearestMonitor.list <- function(raster, longitude, latitude) {
#'   cl <- parallel::makeCluster(future::availableCores() - 1)
#'   future::plan(strategy = future::cluster, workers = cl)
#'   data <- future.apply::future_lapply(
#'     X = raster,
#'     FUN = function(r) {
#'       raster_loadNearestMonitor.Raster(r, longitude, latitude)
#'     }
#'   )
#'   parallel::stopCluster(cl)
#'   return(data)
#' }
