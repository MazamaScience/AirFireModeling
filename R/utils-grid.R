#' @export
#'
#' @title Test for correct structure in a \emph{bs_grid} object
#'
#' @param bs_grid \emph{bs_grid} object
#'
#' @return \code{TRUE} if \code{bs_grid} has the correct structure,
#' \code{FALSE} otherwise.
#'
#' @description The \code{bs_grid} is checked for the 'bs_grid' class name
#' and presence of the following elements:
#' \itemize{
#'   \item{longitude}
#'   \item{latitude}
#'   \item{elevation}
#'   \item{time}
#'   \item{data}
#'   \item{model}
#'   \item{modelRun}
#'   \item{deltaLon}
#'   \item{deltaLat}
#' }
#'
grid_isGrid <- function(
  bs_grid = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(bs_grid) ) return(FALSE)
  if ( !"bs_grid" %in% class(bs_grid) ) return(FALSE)

  elements <- c(
    "longitude", "latitude", "elevation", "time", "data",
    "model", "modelRun", "deltaLon", "deltaLat"
  )

  if ( !all(elements %in% names(bs_grid)) ) return(FALSE)

  # Nothing failed so return TRUE
  return(TRUE)

}


#' @title Find bs_grid bounding box
#'
#' @param bs_grid a bs_grid object
#'
#' @description Quickly find the bounding box of a \code{bs_grid} object.
#' @return a bounding box matrix
grid_boundingBox <- function(bs_grid) {
  
  model_bb <- rbind(
    cbind(min = min(bs_grid$longitude),
          max = max(bs_grid$longitude)),
    cbind(min = min(bs_grid$latitude),
          max = max(bs_grid$latitude))
  )
  
  rownames(model_bb) <- c('x', 'y')
  
  return(model_bb)
  
}



#' @keywords internal
#' @export
#' @title Create a Scaled Range
#' @param x vector of numeric values
#' @param factor scaling factor to be applied
#' @param minRange minimum range to be returned
#' @description Scale the range of values by a given factor. The min and max values of the
#' original range of valus are adjusted so that the new range is larger or smaller by \code{factor}.
#' The returned range will always be equal to or greater than \code{minRange}.
#' @return low and high values of the adjusted range
#' @examples
#' \dontrun{
#' x <- (1:10)
#' range(x)
#' [1] 1 10
#' x_new <- adjustRange(x, factor = 2)
#' range(x_new)
#' [1] -3.5 14.5
#' }
adjustRange <- function(x, factor = 1.1, minRange = 0.1) {
  scaledWidth <- diff(range(x)) * (factor - 1)
  low <- range(x)[1] - scaledWidth / 2
  high <- range(x)[2] + scaledWidth / 2
  if ( (high - low) < minRange ) {
    mid <- (max(x)-min(x))/2 + min(x)
    low <- mid - minRange / 2
    high <- mid + minRange / 2
  }
  return(c(low, high))
}
