#' @export
#' @title Create a Raster from monitor object
#'
#' @param ws_monitor \pkg{PWFSLSMoke} \emph{ws_monitor} object.
#' @param raster Raster* object defining the output grid.
#' @param FUN Function used to collapse values when multiple monitors lie within
#' a single grid cell.
#'
#' @return A \code{RasterBrick} object.
#'
#' @examples
#' \donttest{
#' # library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = c("PNW-4km"),
#'   modelRun = c(2019100900),
#'   xlim = c(-125, -117),
#'   ylim = c(45.5, 49)
#' )
#'
#' # Load monitor data
#' ws_monitor <-
#'   PWFSLSmoke::monitor_load(
#'     startdate = 2019100901,
#'     enddate = 2019101223
#'   ) %>%
#'   PWFSLSmoke::monitor_subset(stateCodes = "WA")
#'
#' monitor_rasterBrick <- monitor_toRaster(ws_monitor, rasterList[[1]], mean)
#'
#' raster_facet(
#'   monitor_rasterBrick,
#'   index = 1:6,
#'   title = "Monitoring data",
#'   palette = 'Spectral',
#'   col_county = 'gray95',
#'   direction = -1,
#'   breaks = c(-Inf, 0, 12, 35, 55, 150, 250, 350, Inf)
#' )
#'}
#'
monitor_toRaster <- function(
  ws_monitor = NULL,
  raster = NULL,
  FUN = mean
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(ws_monitor)
  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(FUN)

  if ( !PWFSLSmoke::monitor_isMonitor(ws_monitor) )
    stop("Parameter 'ws_monitor' is not a valid ws_monitor object.")

  if ( PWFSLSmoke::monitor_isEmpty(ws_monitor) )
    stop("Parameter 'ws_monitor' contains no data.")

  if ( !raster_isRaster(raster) )
    stop("Parameter 'raster' must be a single Raster* object.")

  if ( !rlang::is_closure(FUN) )
    stop("Parameter 'UN' must be a function.")

  # ----- Create SpatialPointsDataFrame ----------------------------------------

  # NOTE:  Prepare coords and data so that rows are locations
  coords <-
    PWFSLSmoke::monitor_extractMeta(ws_monitor) %>%
    dplyr::select("longitude", "latitude")

  data <-
    PWFSLSmoke::monitor_extractData(ws_monitor)[,-1] %>%
    t() %>%
    data.frame()

  # Assign raster package style timestamps
  colnames(data) <- paste0("X", as.numeric(ws_monitor$data$datetime))

  SP <- sp::SpatialPoints(
    coords = coords,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )

  # ----- Rasterize ------------------------------------------------------------

  # Create empty RasterLayer
  rasterLayer <- raster[[1]]
  raster::values(rasterLayer) <- NA

  # Rasterize
  rasterBrick <- raster::rasterize(
    x = SP,
    y = rasterLayer,
    field = data,
    fun = FUN,
    background = NA,
    mask = FALSE,
    update = FALSE,
    updateValue = "all",
    filename = "",
    na.rm = TRUE
  )

  # ----- Return ---------------------------------------------------------------

  return(rasterBrick)

}
