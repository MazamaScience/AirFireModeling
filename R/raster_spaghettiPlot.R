#' @export
#' @title Spaghetti Plot
#'
#' @param raster A Raster\* object or a list of Raster\* objects.
#' @param longitude Target longitude from which the radius will be calculated.
#' @param latitude Target latitude from which the radius will be calculated.
#' @param radius Distance (km) of radius from target location.
#' @param count Number of grid cells within radius to return.
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
#' \dontrun{
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
#'   model = c("PNW-1.33km", "PNW-4km"),
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
#'
#' }
raster_spaghettiPlot <- function(
  raster = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = 5,
  count = NULL,
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

  # Defaults
  if ( !is.logical(verbose) ) verbose <- TRUE

  # ----- Prepare data ---------------------------------------------------------

  monitorList <- raster_toMonitor(
    raster,
    longitude = longitude,
    latitude = latitude,
    radius = radius,
    count = count,
    verbose = verbose
  )

  # ----- Prepare default plot arguments ---------------------------------------

  argsList <- list(...)

  if ( !('type' %in% names(argsList)) )
    argsList$type <- 'l'

  if ( !('lwd' %in% names(argsList)) )
    argsList$lwd <- 2

  if ( !('localTime' %in% names(argsList)) )
    argsList$localTime <- TRUE

  if ( !('shadedNight' %in% names(argsList)) )
    argsList$shadedNight <- TRUE

  if ( !('ylab' %in% names(argsList)) )
    argsList$ylab <- "PM2.5"

  if ( !('main' %in% names(argsList)) )
    argsList$main <- "PM2.5"

  if ( !('add' %in% names(argsList)) )
    argsList$add <- FALSE

  # Always use UTC to avoid multiple-timezone issues
  if ( !('add' %in% names(argsList)) )
    argsList$localTime <- FALSE

  # Awkward chain to get min and max datetimes from monitorList
  tlim <-
    lapply(monitorList, function(x) { return(range(x$data$datetime)) }) %>%
    unlist() %>%
    as.numeric() %>%
    range() %>%
    as.POSIXct(tz = "UTC", origin = lubridate::origin)

  # NOTE:  Because of a bug in monitor_timeseriesPlot(), we need to specify
  # NOTE:  tlim in the local timezone if we want to display

  # TODO:  Improve decision on choosing a timezone
  if ( argsList$localTime ) {
    timezone <- monitorList[[1]]$meta$timezone[1]
    tlim <- lubridate::with_tz(tlim, tzone = timezone)
  }

  # Get ylim from the range of data values
  if ( is.null(ylim) ) {
    ymax <-
      sapply(monitorList, function(x) { return(max(x$data[,-1], na.rm = TRUE)) }) %>%
      max(na.rm = TRUE)

    ylim <- c(0, ymax)
  }

  # ----- Create the plot ------------------------------------------------------

  if ( 'col' %in% names(argsList) ) {
    colors <- rep(argsList$col, times = length(monitorList))
  } else {
    colors <- RColorBrewer::brewer.pal(12, "Paired")
  }

  # Create a blank plot first to get the x and y limits
  if ( !argsList$add ) {

    graphics::plot(
      x = tlim[1],
      y = 0,
      col = 'transparent',
      ylab = argsList$ylab,
      main = argsList$main,
      xlim = tlim,
      ylim = ylim,
      las = 1
    )

  }

  # NOTE:  Having created a blank plot we have to add our own shaded night
  # NOTE:  with code borrowed from PWFSLSmoke::monitor_timeseriesPlot().

  if ( argsList$shadedNight ) {
    times <- seq(tlim[1], tlim[2], by = "hour")
    lat <- mean(monitorList[[1]]$meta$latitude)
    lon <- mean(monitorList[[1]]$meta$longitude)
    timeInfo <- PWFSLSmoke::timeInfo(times, lon, lat, timezone)
    PWFSLSmoke::addShadedNight(timeInfo)
  }

  # All subsequent plots are added
  argsList$add <- TRUE

  for ( i in seq_along(monitorList) ) {

    argsList$ws_monitor <- monitorList[[i]]
    argsList$col <- colors[i]

    # # First plot
    # if ( i == 1 && !('add' %in% names(argsList)) )
    #   argsList$add <- FALSE
    #
    # if ( i > 1 )
    #   argsList$add <- TRUE

    # Plot
    do.call(PWFSLSmoke::monitor_timeseriesPlot, argsList)

  }

  graphics::legend(
    "topright",
    legend = names(monitorList),
    col = colors[1:length(monitorList)],
    lwd = argsList$lwd
  )

}

