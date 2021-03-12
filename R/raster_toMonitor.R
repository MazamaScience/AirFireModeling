#' @export
#' @title Create a PWFSLSmoke ws_monitor object from a Raster\* object
#'
#' @param raster A Raster* object.
#' @param longitude Target longitude from which the radius will be calculated.
#' @param latitude Target latitude from which the radius will be calculated.
#' @param radius Distance (km) of radius from target location.
#' @param count Number of grid cells within radius to return.
#' @param rasterName Optional string prepended to monitorIDs.
#' @param verbose Logical to display messages.
#'
#' @description Time series associated with multiple grid cells are converted
#' into a \pkg{PWFSLSmoke} \emph{ws_monitor}. The \code{monitorID} associated
#' with each location is generated with:
#'
#' \preformatted{
#'   MazamaLocationUtils::location_createID(x, y)
#' }
#'
#' @return A \pkg{PWFSLSmoke} \emph{ws_monitor} object representing a single
#' monitor.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = c("PNW-1.33km", "PNW-4km"),
#'   modelRun = c(2020091300),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' # MazamaSpatialUtils are needed to determine timezone and state
#' library(MazamaSpatialUtils)
#' PWFSLSmoke::initializeMazamaSpatialUtils()
#'
#' model_Portland <- raster_toMonitor(
#'   rasterList,
#'   longitude = -122.68,
#'   latitude = 45.52,
#'   radius = 5
#' )
#'
#' PWFSLSmoke::monitor_timeseriesPlot(
#'   model_Portland[[1]],
#'   type = 'l', col = 'salmon'
#' )
#' PWFSLSmoke::monitor_timeseriesPlot(
#'   model_Portland[[2]],
#'   type = 'l', col = 'dodgerblue', add = TRUE
#' )
#'
#' legend(
#'   x = "topright",
#'   legend = names(model_Portland),
#'   lwd = c(1, 1),
#'   col= c("salmon", "dodgerblue")
#' )
#' }
#'
raster_toMonitor <- function(
  raster,
  longitude = NULL,
  latitude = NULL,
  radius = 5,
  count = NULL,
  rasterName = NULL,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)

  if ( !is.list(raster) && !raster_isRaster(raster) )
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

  # Check domain
  if ( is.list(raster) ) {
    r <- raster[[1]]
  } else {
    r <- raster
  }

  if ( longitude < raster::xmin(r) | longitude > raster::xmax(r) |
       latitude < raster::ymin(r)  |  latitude > raster::ymax(r) ) {
    stop('Check Coordinates: Target location is outside the raster domain.')
  }

  # Defaults
  if ( !is.character(rasterName) ) rasterName <- "raster"
  if ( !is.logical(verbose) ) verbose <- TRUE

  # Guarantee rasterName is not mult-valued
  rasterName <- rasterName[1]

  # Check if source-file path is specified in raster object
  # if ( identical(raster@file@name,"") || is.null(raster@file@name) ) {
  #   stop("Missing NC path: Check the raster file path.")
  # }

  # ----- Subset the Raster(s) -------------------------------------------------

  if ( is.list(raster) ) {

    names <- names(raster)
    monitorList <- list()

    for ( i in seq_along(raster) ) {
      name <- names[i]
      if ( verbose )
        message(sprintf("Converting %s to ws_monitor ...", name))
      monitorList[[name]] <-
        .toMonitor(
          raster = raster[[i]],
          longitude = longitude,
          latitude = latitude,
          radius = radius,
          count = count,
          rasterName = name
        )
    }

    return(monitorList)

  } else {

    if ( verbose )
      message(sprintf("Converting raster to ws_monitor ..."))

    monitor <-
      .toMonitor(
        raster,
        longitude = longitude,
        latitude = latitude,
        radius = radius,
        count = count,
        rasterName = rasterName
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
  rasterName = NULL
) {

  # Parameter validation handled in calling funcitions

  # ----- Prepare meta ---------------------------------------------------------

  # Subset by distance
  localRaster <- raster_subsetByDistance(
    raster,
    longitude = longitude,
    latitude = latitude,
    radius = radius,
    count = count
  )

  # Extract coordinates
  coords <- raster::coordinates(localRaster)

  # Generate unique monitorIDs
  cellMonitorIDs <- mapply(
    function(x,y) {
      ###sprintf("mon_%.4f_%.4f", x, y)
      MazamaLocationUtils::location_createID(x, y)
    },
    coords[,1],
    coords[,2]
  )

  cellMonitorIDs <- make.names(sprintf("%s_%s", rasterName, cellMonitorIDs))

  # Create "meta"
  meta <- PWFSLSmoke::createEmptyMetaDataframe(rows = nrow(coords))
  meta$monitorID <- cellMonitorIDs
  meta$longitude <- coords[,"x"]
  meta$latitude <- coords[,"y"]
  meta$monitorType <- "MODEL_DATA"
  rownames(meta) <- meta$monitorID

  # Extract modelName and modelRun from, e.g. "PNW-4km_2019100900"
  # Returns NA if a custom rasterName is passed in.
  parts <- stringr::str_match(rasterName, "(.*)_([0-9]+)")
  meta$modelName <- parts[1,2]
  meta$modelRun <- parts[1,3]

  # Add distance from target (km)
  meta$targetDistance <- geosphere::distGeo(c(longitude, latitude), coords) / 1000

  # Add timezone, stateCode, countryCode
  meta <- PWFSLSmoke::addMazamaMetadata(meta)

  # ----- Prepare data ---------------------------------------------------------

  # Create POSIXct times from the Raster layers
  times <- raster_generateTime(localRaster)

  # Extract values from Raster Object at the target spatial point(s)
  localValues <- raster::getValues(localRaster)

  # Remove targetData names and transpose
  colnames(localValues) <- NULL
  localData <- t(localValues)

  # Add monitorID names
  colnames(localData) <- cellMonitorIDs

  # Create "data"
  data <- cbind(
    data.frame(datetime = times),
    data.frame(localData)
  )

  # ----- Return 0--------------------------------------------------------------

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
    modelName = "PNW-4km",
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

}
