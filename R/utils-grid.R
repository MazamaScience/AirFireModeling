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
