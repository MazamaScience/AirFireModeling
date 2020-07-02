#' @export
#' @title Spaghetti Plot
#'
#' @param raster A Raster\* object or a list of Raster\* objects.
#' @param longitude Target longitude from which the radius will be calculated.
#' @param latitude Target latitude from which the radius will be calculated.
#' @param radius Distance (km) of radius from target location.
#' @param count Number of grid cells within radius to return.
#' @param rasterName Name used when \code{raster} is a \code{RasterBrick}.
#' @param verbose Logical to display messages.
#' @param ylim Optional Y axis limits.
#' @param ... Additional arguments passed to \code{PWFSLSmoke::monitor_timeseriesPlot()}.
#'
#' @description Plot a "spaghetti plot" of model data near a target location.
#'
#' For each Raster\* object in \code{raster}, cells near the target location
#' are extracted using \code{raster_subsetByDistance()}. These are converted
#' into individual \pkg{PWFSLSMoke} \emph{ws_monitor} objects using
#' \code{raster_toMonitor()} and then plotted.
#'
#' @seealso \code{\link{raster_subsetByDistance}}
#' @seealso \code{\link{raster_toMonitor}}
#' @seealso \code{\link[PWFSLSmoke]{monitor_timeseriesPlot}}
#'
#' @return A ggplot object.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Creating PWFSLSmoke ws_monitor objects requires:
#' library(MazamaSpatialUtils)
#' PWFSLSmoke:::initializeMazamaSpatialUtils()
#'
#' # Portland, Oregon
#' longitude <- -122.68
#' latitude <- 45.52
#'
#' models <- bluesky_findModels(longitude, latitude)
#' # > models
#' # [1] "NAM84-0.15deg"                    "GFS-0.15deg-CanadaUSA-p25deg-68N"
#' # [3] "NAM-3km"                          "CANSAC-4km"
#' # [5] "PNW-4km"                          "PNW-1.33km"
#'
#' # We will only use a subset for this example
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = c("PNW-1.33km", "PNW-4km"),
#'   modelRun = 2019100900,
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_spaghettiPlot(
#'   rasterList,
#'   longitude = longitude,
#'   latitude = latitude,
#'   radius = 10 # km
#' )
#' }

raster_spaghettiPlot <- function(
  raster = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = 5,
  count = NULL,
  rasterName = NULL,
  verbose = TRUE,
  ylim = NULL,
  ...
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

  # Check that rasterName is specified when raster is not a list
  if ( raster_isRaster(raster) && is.null(rasterName) ) {
    stop(paste0(
      "When parameter 'raster' is a RasterBrick, the additional parameter ",
      "'rasterName' must be specified."
    ))
  }

  if ( is.list(raster) && is.null(names(raster)) ) {
    stop(paste0(
      "When parameter 'raster' is a list, it must have names."
    ))
  }

  # Defaults
  if ( !is.logical(verbose) ) verbose <- TRUE

  # ----- Prepare data ---------------------------------------------------------

  monitorList <- raster_toMonitor(
    raster,
    longitude = longitude,
    latitude = latitude,
    radius = 10,
    count = count,
    rasterName = rasterName,
    verbose = verbose
  )

  # ----- Prepare default plot arguments ---------------------------------------

  # Internal Function
  .meltTargetMonitor <- function(monitorList) {
    # Melt the data
    moltenTargetDist <-
      monitorList$meta %>%
      dplyr::select(.data$monitorID, .data$targetDistance) %>%
      reshape2::melt(id.vars = 'monitorID') %>%
      dplyr::mutate(variable = .data$monitorID, 'monitorID' = NULL) %>%
      dplyr::mutate(modelName = stringr::str_remove(.data$variable, '_.+_.+'))

    moltenData <-
      monitorList$data %>%
      reshape2::melt(measure.vars = monitorList$meta$monitorID)

    # Merge to data.frame and group by datetime
    df <- merge(moltenData, moltenTargetDist, by = 'variable') %>%
      dplyr::group_by(.data$datetime)

    names(df) <- c('monitorID', 'datetime', 'pm25', 'dist', 'model')

    return(df)
  }

  # Create ggplot dataframes
  if ( stringr::str_detect(class(monitorList), 'Raster*') ) {
    df <- .meltTargetMonitor(monitorList)
  } else {
    df <- lapply(monitorList, .meltTargetMonitor) %>%
      dplyr::bind_rows()

  }

  # ----- ggplot ---------------------------------------------------------------

  gg <-
    ggplot2::ggplot(
      data = df,
      ggplot2::aes(
        x = .data$datetime,
        y = .data$pm25,
        group = .data$monitorID,
        alpha = -(.data$dist),
        modelName = .data$model
      )
    ) +
    ggplot2::geom_line(color = 'firebrick2') +
    ggplot2::geom_point(color = 'firebrick2', shape = 15 ) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$model)) +
    ggplot2::labs(
      title = paste0(latitude, ' \U00B0 N ',longitude, ' \U00B0 W ', rasterName),
      subtitle = ifelse(
        !is.null(radius),
        paste0('Cells within ', radius, ' km'),
        paste0(count, ' Adjacent cells')
      ),
      x = 'Date',
      y = 'PM2.5'
    ) +
    ggplot2::theme(legend.position = 'none')

  return(gg)

}


# ===== DEBUGING ===============================================================

if ( FALSE ) {

  setModelDataDir('~/Data/BlueSky/')


  longitude = -119
  latitude = 35
  radius = 5
  count = NULL
  rasterName = NULL
  verbose = TRUE
  ylim = c(34,37)
  xlim = c(-118, -120)

  bs1 <- bluesky_load(modelName = c('CANSAC-4km'), modelRun = 2020040100, ylim = ylim, xlim = xlim)
  # bs2 <- bluesky_load(modelName = c('CANSAC-1.33km'), modelRun = 2020040100)

  raster_spaghettiPlot(bs1, longitude, latitude)

}


