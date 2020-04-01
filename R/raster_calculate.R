#' #' @title Calculate Values
#' #'
#' #' @description A wrapper around \code{raster::calc()}. Preform calculations for a
#' #' new Raster* object from another raster using a formula.
#' #'
#' #' @param raster A Raster\* object or a list of Raster\* objects.
#' #' @param FUN A function to apply to each RasterLayer (i.e. model time)
#' #'
#' #' @return A Raster* object
#' #' @export
#' #'
#' raster_calculate <- function(raster, FUN) {
#'   if ( class(raster) == 'list' ) {
#'     cl <- parallel::makeCluster(future::availableCores() - 1)
#'     future::plan(strategy = future::cluster, workers = cl)
#'     output <- future.apply::future_lapply(
#'       X = raster,
#'       FUN = function(x) raster::calc(x, FUN)
#'     )
#'     parallel::stopCluster(cl)
#'   } else if ( stringr::str_detect(class(raster), 'Raster*') ) {
#'     output <- raster::calc(raster, FUN)
#'   } else {
#'     stop('Invalid raster object.')
#'   }
#'   return(output)
#' }
