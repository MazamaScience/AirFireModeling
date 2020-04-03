#' #' @title Plot a raster coordinate-temporal values
#' #'
#' #' @param raster A Raster\* object or a list of Raster\* objects.
#' #' @param longitude (Optional) Target longitude to obtain values.
#' #' @param latitude (Optional) Target latitude to obtain values.
#' #' @param monitorID (Optional) a monitor ID to load
#' #' @param tlim (Optional) a time limit to restrict the plot axis.
#' #' @param ... additional parameters. See details.
#' #'
#' #' @return A ggplot object
#' #' @export
#' #'
#' raster_coordinateTrace <- function( raster,
#'                                     longitude = NULL,
#'                                     latitude = NULL,
#'                                     monitorID = NULL,
#'                                     tlim = 'default',
#'                                     ... ) {
#'   UseMethod('raster_coordinateTrace', raster)
#' }
#'
#' ## If the monitor id is null, do not download a target monitor and return ??
#' ## If the monitor id is nearest, download the nearest model and combine
#' ## else if monitor id is defined and valid, download model and combine
#'
#' #' @importFrom stats  median
#' #' @describeIn raster_coordinateTrace A multithreaded implementation for lists of Raster* objects.
#' #' @export
#' raster_coordinateTrace.list <- function( raster,
#'                                          longitude = NULL,
#'                                          latitude = NULL,
#'                                          monitorID = NULL,
#'                                          tlim = 'default',
#'                                          ... ) {
#'
#'   cl <- parallel::makeCluster(future::availableCores() - 1)
#'   future::plan(strategy = future::cluster, workers = cl)
#'   monitors <- future.apply::future_lapply(
#'     X = raster,
#'     FUN = function(r) {
#'       tryCatch(
#'         expr = {
#'           .load_target_monitor(r, longitude, latitude, monitorID)
#'         },
#'         error = function(e) {
#'           PWFSLSmoke::createEmptyMonitor()
#'         }
#'       )
#'     }
#'   )
#'
#'   parallel::stopCluster(cl)
#'
#'   # NOTE: Hacky solution to split and recombine to avoid errors with combining multiple
#'   # identical target monitors.
#'   target_monitor <- PWFSLSmoke::monitor_subsetBy(
#'     monitors[[1]],
#'     monitorID == monitors[[1]]$meta$monitorID[2]
#'   )
#'   # Split the model monitor from the target monitor
#'   model_monitors <- lapply(
#'     X = monitors,
#'     FUN = function(x) {
#'       tryCatch(
#'         expr = {
#'           PWFSLSmoke::monitor_subsetBy(
#'             x,
#'             monitorID != target_monitor$meta$monitorID
#'             )
#'         },
#'         error = function(e) {
#'           PWFSLSmoke::createEmptyMonitor()
#'         }
#'       )
#'     }
#'   )
#'   model_monitors <- PWFSLSmoke::monitor_combine(model_monitors)
#'
#'   # Re-combine the target monitor and model monitor
#'   ws_data <- PWFSLSmoke::monitor_combine(list(target_monitor, model_monitors))
#'
#' # Handle time axis cropping
#' if ( !is.null(tlim) )
#'   # The 'default" tlim is determined by the median nlayers of the rasters supplied
#'   # NOTE: find an alternative
#'   if ( tlim == 'default' ) {
#'     nlayers <- median(unlist(lapply(
#'       X = raster,
#'       FUN = function(r) {
#'         raster::nlayers(r)
#'       } )))
#'     startdate <- lubridate::ymd(strftime(ws_data$data$datetime[1], '%Y%m%d'))
#'     enddate <- startdate + lubridate::hours(nlayers) + lubridate::days(1) # end on the end of the last day
#'     ws_data <- PWFSLSmoke::monitor_subset( ws_data,
#'                                            tlim = c(strftime(startdate, '%Y%m%d'),
#'                                                     strftime(enddate, '%Y%m%d')) )
#'   } else {
#'     ws_data <- PWFSLSmoke::monitor_subset(ws_data, tlim = tlim)
#'   }
#'
#'   gg <- AirMonitorPlots::ggplot_pm25Timeseries(ws_data) +
#'     AirMonitorPlots::geom_pm25Points(ggplot2::aes(color = .data$monitorID)) +
#'     AirMonitorPlots::stat_nowcast(ggplot2::aes(color = .data$monitorID)) +
#'     ggplot2::labs(color = '') +
#'     ggplot2::theme( legend.position = c(1,1),
#'                     legend.margin = ggplot2::margin(),
#'                     legend.justification = c(1, 1),
#'                     legend.direction = 'vertical',
#'                     legend.box = "vertical",
#'                     legend.background = ggplot2::element_rect(fill = 'transparent',
#'                                                               color = 'transparent') )
#'
#'   return(gg)
#' }
#'
#' #' @describeIn raster_coordinateTrace A Raster* object implementation.
#' #' @export
#' raster_coordinateTrace.Raster <- function( raster,
#'                                            longitude,
#'                                            latitude,
#'                                            monitorID = NULL,
#'                                            ... ) {
#'   ws_data <- .load_target_monitor(raster, longitude, latitude, monitorID)
#'
#'   gg <- AirMonitorPlots::ggplot_pm25Timeseries(ws_data) +
#'     AirMonitorPlots::geom_pm25Points(ggplot2::aes(color = .data$monitorID)) +
#'     AirMonitorPlots::stat_nowcast(ggplot2::aes(color = .data$monitorID)) +
#'     ggplot2::labs(color = '') +
#'     ggplot2::theme( legend.position = c(1,1),
#'                     legend.margin = ggplot2::margin(),
#'                     legend.justification = c(1, 1),
#'                     legend.direction = 'vertical',
#'                     legend.box = "vertical",
#'                     legend.background = ggplot2::element_rect(fill = 'transparent',
#'                                                               color = 'transparent') )
#'
#'   return(gg)
#'
#' }
#'
#' # NOTE: Look into a solution to avoid having to reload the monitor_load on execution
#' # Sub-internal function to load a monitor using the model.
#' .load_target_monitor <- function(r, lon = NULL, lat = NULL, monitorID = NULL, ws_monitor = NULL,  ...) {
#'   # Check if lon lat is in coordinate domain
#'   if ( !is.null(lat) && !is.null(lon) ) {
#'     if ( abs(lon) < abs(r@extent@xmax) ||
#'          abs(lon) > abs(r@extent@xmin) ) {
#'       stop('Longitude not within domain.')
#'     }
#'     if ( abs(lat) > abs(r@extent@ymax) ||
#'          abs(lat) < abs(r@extent@ymin) ) {
#'       stop('Latitude not within domain.')
#'     }
#'   }
#'   # Parse dates stored in model
#'   # NOTE: model dates stored in layer name
#'   model_dates <- as.numeric(stringr::str_remove(r@data@names, pattern = 'X'))
#'   class(model_dates) <- c('POSIXct', 'POSIXt')
#'   attr(model_dates, 'tzone') <- 'UTC'
#'   # Load All monitors for model dates
#'   startdate <- range(model_dates)[1]
#'   enddate <- range(model_dates)[2]
#'
#'   if ( is.null(ws_monitor) ) {
#'     ws_monitor <- PWFSLSmoke::monitor_load(startdate, enddate)
#'   }
#'
#'   # If Monitor ID is not provided, load nearest one unless specified otherwise
#'   if ( is.null(monitorID) ) {
#'
#'     monitors_dist <- geosphere::distHaversine( cbind(ws_monitor$meta$longitude,
#'                                                      ws_monitor$meta$latitude),
#'                                                cbind(lon, lat) )
#'     nearest_monitorID <- ws_monitor$meta$monitorID[which.min(monitors_dist)]
#'     target_dist <- monitors_dist[which.min(monitors_dist)]
#'     # TODO: Incorperate the target distance to show on plots
#'     target_monitor <- PWFSLSmoke::monitor_subset(ws_monitor,
#'                                                  monitorIDs = nearest_monitorID)
#'
#'   } else {
#'     target_monitor <- PWFSLSmoke::monitor_subset(ws_monitor,
#'                                                  monitorIDs = monitorID)
#'   }
#'
#'   # Overwrite coordinates unless specified
#'   longitude <- ifelse(is.null(lon), target_monitor$meta$longitude, lon)
#'   latitude <- ifelse(is.null(lat), target_monitor$meta$latitude, lat)
#'
#'   model_name <- stringr::str_extract(r@file@name, '(?<=[/])([^/]+)(?=\\_[^.]+)')
#'   model_monitor <- raster_toMonitor(r, longitude, latitude, monitorID = model_name)
#'   combined <- PWFSLSmoke::monitor_combine(list(model_monitor, target_monitor))
#'   return(combined)
#' }
