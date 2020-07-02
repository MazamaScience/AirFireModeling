#' @export
#' @title Subset a Raster* by polygon
#'
#' @param raster Raster\* object.
#' @param polygon Spatial\* object.
#'
#' @description This function subsets a raster object using cookie cutter
#' defined by \code{polygon}. All cells within the polygon are returned.
#'
#' @return A Raster* object.
#'
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = "PNW-4km",
#'   modelRun = c(2019100800, 2019100900, 2019101000, 2019101100),
#'   xlim = c(-125, -116.5),
#'   ylim = c(42, 46.2)
#' )
#'
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#'
#' OregonList <- raster_subsetByPolygon(
#'   rasterList,
#'   polygon = subset(USCensusStates, stateCode == "OR")
#' )
#'
#' raster_ggmap(
#'   OregonList,
#'   palette = "Spectral",
#'   breaks = c(-1, 0, 1, 2, 4, 8, 16, 32, Inf),
#'   direction = -1
#' )
#' }
raster_subsetByPolygon <- function(
  raster = NULL,
  polygon = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(polygon)

  if ( !is.list(raster) && !raster_isRaster(raster) )
    stop("Parameter 'raster' must be a single or a list of Raster* objects.")

  if ( !stringr::str_detect(as.character(class(polygon)), "^Spatial") )
    stop("Parameter 'polygon' must be a Spatial* object.")

  # ----- Subset the Raster(s) -------------------------------------------------

  if ( raster_isRaster(raster) ) {

    rasterBrick <- suppressWarnings({
      raster::mask(raster, polygon)
    })

    return(rasterBrick)

  } else {

    rasterList <- lapply(
      X = raster,
      FUN = function(r) {
        suppressWarnings({
          raster::mask(r, polygon)
        })
      }
    )

    return(rasterList)

  }

}

