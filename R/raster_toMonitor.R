#' @export
#' @title Create a PWFSLSmoke ws_monitor object from raster object
#'
#' @param raster A Raster* object.
#' @param longitude Target longitude from which the radius will be calculated.
#' @param latitude Target latitude from which the radius will be calculated.
#' @param radius Distance (km) of radius from target location.
#' @param count Number of grid cells within radius to return.
#' @param FUN A function to collapse raster cells into a single timeseries.
#' @param na.rm Logical value passed as an argument to \code{FUN}.
#' @param monitorID Character identifer for the created \emph{ws_monitor} object.
#'
#' @description Time series associated with multiple grid cells are merged into
#' a single time series by using \code{FUN} to collapse all cells (up to
#' \code{count}) within \code{radius} km of the target location. For example,
#' if \code{FUN = mean} then the selected grid cells are averaged to
#' a single central coordinate.
#'
#' @details If \code{raster} is a \code{RasterList}, names from the list will
#' be used as \code{monitorID}. If \code{raster} is of class \code{RasterBrick},
#' the user is expected to supply a \code{monitorID}.
#'
#' @return A \pkg{PWFSLSmoke} \emph{ws_monitor} object representing a single
#' monitor.
#'
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load from server
#' rasterList <- raster_load(
#'   model = "PNW-4km",
#'   modelRun = c(2019100900),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' # MazamaSpatialUtils are needed to determine timezone and state
#' PWFSLSmoke::initializeMazamaSpatialUtils()
#'
#' fake_Portland <- raster_toMonitor(
#'   rasterList[[1]],
#'   longitude = -122.68,
#'   latitude = 45.52,
#'   radius = 10,
#'   monitorID = "fake_Portland"
#' )
#'
#' PWFSLSmoke::monitor_timeseriesPlot(fake_Portland)
#' }
#'
raster_toMonitor <- function(
  raster,
  longitude = NULL,
  latitude = NULL,
  radius = 5,
  count = NULL,
  FUN = mean,
  na.rm = TRUE,
  monitorID = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)

  if ( class(raster) != "list" &&
       !stringr::str_detect(class(raster), 'Raster*') )
    stop("Parameter 'raster' must be a single or a list of Raster* objects.")

  if ( !is.numeric(longitude) )
    stop("Parameter 'longitude' must be numeric.")

  if ( !is.numeric(latitude) )
    stop("Parameter 'latitude' must be numeric.")

  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be numeric.")

  if ( !is.null(count) ) {
    if ( !is.numeric(count) )
      stop("Parameter 'count' must be numeric.")
  }

  # Create a random monitorID if needed
  if ( is.null(monitorID) ) {
    if ( stringr::str_detect(class(raster), 'Raster*') )
      monitorID <- basename(tempfile("monitor_"))
  }

  # Check domain
  if ( class(raster) == 'list' ) {
    r <- raster[[1]]
  } else {
    r <- raster
  }

  if ( longitude < raster::xmin(r) | longitude > raster::xmax(r) |
       latitude < raster::ymin(r)  |  latitude > raster::ymax(r) ) {
    stop('Check Coordinates: Target location is outside the raster domain.')
  }

  # ----- Subset the Raster(s) -------------------------------------------------

  if ( class(raster) == 'list' ) {

    names <- names(raster)
    monitorList <- list()
    for ( i in seq_along(raster) ) {
      name <- names[i]
      monitorList[[name]] <-
        .toMonitor(
          raster = raster[[i]],
          longitude = longitude,
          latitude = latitude,
          radius = radius,
          count = count,
          FUN = FUN,
          na.rm = na.rm,
          monitorID = name
        )
    }

    return(monitorList)

  } else if ( stringr::str_detect(class(raster), 'Raster*') ) {

    monitor <-
      .toMonitor(
        raster,
        longitude = longitude,
        latitude = latitude,
        radius = radius,
        count = count,
        FUN = FUN,
        na.rm = na.rm,
        monitorID = monitorID
      )

    return(monitor)

  }

}

# ===== Internal Functions =====================================================

# Internal function to convert a rasterBrick to a ws_monitor object
.toMonitor <- function(
  raster,
  longitude = NULL,
  latitude = NULL,
  radius = 5,
  count = NULL,
  FUN = mean,
  na.rm = TRUE,
  monitorID = NULL
) {

  # Parameter validation handled in calling funcitions

  # ----- Prepare data ---------------------------------------------------------

  # Create target Spatial Point
  target_sp <- sp::SpatialPoints(
    coords = cbind(longitude, latitude),
    proj4string = raster::crs(raster)
  )

  # Extract values from Raster Object at the target spatial point(s)
  targetData <- c(t(raster::extract(
    x = raster,
    y = target_sp,
    buffer = radius * 1000, # radius is always in km in AirFireModeling
    fun = FUN
  )))

  layerTimes <- createLayerTime(names(raster))

  # ----- Create ws_monitor object and populate --------------------------------

  # Fill Meta
  meta <- PWFSLSmoke::createEmptyMetaDataframe(rows = 1)
  meta$monitorID <- as.character(monitorID)
  meta$longitude <- as.numeric(longitude)
  meta$latitude <- as.numeric(latitude)
  rownames(meta) <- as.character(monitorID)

  # Add timezone, stateCode, countryCode
  meta <- PWFSLSmoke::addMazamaMetadata(meta)

  # Fill Data
  data <- data.frame(layerTimes, targetData)
  colnames(data) <- c('datetime', monitorID)

  # Combine into ws_monitor list object
  ws_monitor <- list('meta' = meta, 'data' = data)
  class(ws_monitor) <- c('ws_monitor', class(ws_monitor))

  return(ws_monitor)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(AirFireModeling)
  setModelDataDir('~/Data/BlueSky')

  # Load from server
  rasterList <- raster_load(
    model = "PNW-4km",
    modelRun = c(2019100900),
    xlim = c(-125, -115),
    ylim = c(42, 50)
  )

  raster = rasterList[[1]]
  # Portland
  longitude = -122.68
  latitude = 45.52
  radius = 5
  count = NULL
  FUN = mean
  na.rm = TRUE
  monitorID = NULL

}
