#' @title Plot maps of BlueSky model output
#'
#' @description Creates a \pkg{ggplot2} plot object from a \code{Raster\*}
#' object. The returned plot object can be plotted or can be enhanced with
#' additional \pkg{ggplot2} intructions.
#'
#' @details If a list of Raster\* objects is provided, a small-multiples plot
#' is created.
#'
#' The \code{index} is typically associated with a time-axis or RasterLayer,
#' e.g \code{index = 1} is the first hour of a model.
#'
#' @param raster A Raster\* object or a list of Raster\* objects.
#' @param index The index of the Raster* object. See details.
#' @param palette The color palette used to map cell values. This must be one
#' of the palettes available through \code{ggplot2::scale_colour_brewer()}.
#' @param breaks The breaks used to define the map cell color or \code{'default'}.
#' @param title (Optional) A plot title.
#' @param direction Numeric. \code{direction = -1} reverses color palette.
#' @param timezone The timezone to parse the dates. Default is UTC.
#' @param verbose Logical to display messages.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load from server
#' rasterList <- raster_load(
#'   model = "PNW-4km",
#'   modelRun = c(2019100800, 2019100900, 2019101000, 2019101100),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_map(rasterList, title="PNW-4km", index = 3)
#' }
raster_map <- function(
  raster,
  index = 1,
  palette = 'Greys',
  breaks = 'default',
  title = 'PM2.5',
  direction = 1,
  timezone = 'UTC',
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)

  if ( !is.numeric(index) )
    stop("Parameter 'index' must be numeric.")

  availablePalettes <- rownames(RColorBrewer::brewer.pal.info)
  if ( !palette %in% availablePalettes )
    stop(sprintf("'%s' is not a recognized palette. Please see ?ggplot2::scale_colour_brewer."))

  # breaks

  if ( !is.character(title) )
    stop("Parameter 'title' must be character.")

  if ( !direction %in% c(-1,1) )
    stop("Parameter 'direction' must be either 1 or -1.")

  if ( !timezone %in% OlsonNames())
    stop("Parameter 'timezone' is not recognized. Please see ?OlsonNames/.")

  # ----- Dispatch method ------------------------------------------------------

  UseMethod('raster_map', raster)

}

# ===== Method Disptach ========================================================

# NOTE:  For an explanation of S3 method dispatch, see:
# NOTE:    http://adv-r.had.co.nz/OO-essentials.html#s3

#' @describeIn raster_map Method for Raster* objects.
#' @export
raster_map.Raster <- function(
  raster, # a single RasterBrick
  index = 1,
  palette = 'Greys',
  breaks = 'default',
  title = 'PM2.5',
  direction = 1,
  timezone = 'UTC',
  verbose = TRUE
) {

  if ( index > dim(raster)[3] )
    stop('Index out of range.')

  # NOTE:  Use list syntax to pull a RasterLayer out of a RasterBrick

  layer <- raster[[index]]
  .plot_map(layer, palette, breaks, title, direction, timezone)

}

#' @describeIn raster_map Multi-threaded method for a list of Raster* objects.
#' @export
raster_map.list <- function(
  raster, # a list of RasterBricks
  index = 1,
  palette = 'Greys',
  breaks = 'default',
  title = 'PM2.5',
  direction = 1,
  timezone = 'UTC',
  verbose = TRUE
) {

  if ( verbose )
    message(paste0('Plotting ', length(raster), ' models ...'))

  # Pull individual RasterBrick objects out of the list and subset to create
  # a list of RasterLayer objects.
  layerList <- lapply(
    X = raster,
    FUN = function(r) {
      if ( index > dim(r)[3] ) {
        stop('Index out of range.')
      }
      return(r[[index]])
    }
  )

  # Set up parallelization
  cl <- parallel::makeCluster(future::availableCores() - 1)
  future::plan(strategy = future::cluster, workers = cl)

  # Parallel plotting
  gg_list <- future.apply::future_lapply(
    X = layerList,
    FUN = function(layer) {
      .plot_map(layer, palette, breaks, title, direction, timezone)
    }
  )

  parallel::stopCluster(cl)

  # Assemble individual plots into a grid
  return(cowplot::plot_grid(plotlist = gg_list))

}

# ===== Internal Functions =====================================================

# NOTE: Internal function for plotting an individual ggplot2 raster layer
.plot_map <- function(
  layer,
  palette,
  breaks,
  title,
  direction,
  timezone
) {

  # ----- Prepare data ---------------------------------------------------------

  # Get layer coordinate limits
  limits <- raster::extent(layer)
  xlim <- c(limits@xmin, limits@xmax)
  ylim <- c(limits@ymin, limits@ymax)

  epochSecs <- as.numeric(stringr::str_remove(names(layer), 'X'))
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)
  timeString <- strftime(layerTime, format = "%Y-%m-%d %H:00 %Z", tz = timezone)

  # Define color breaks to match those used by AirFire
  if ( breaks == 'default' ) {
    breaks <- c(0, 12, 35, 55, 150, 250, 350, 500)
  }

  # Get state and counties data for plotting
  states <- ggplot2::map_data('state', xlim = xlim, ylim = ylim)
  counties <- ggplot2::map_data('county', xlim = xlim, ylim = ylim)

  # ----- Create plot ----------------------------------------------------------

  gg <-
    rasterVis::gplot(layer) +
    ggplot2::geom_raster(ggplot2::aes(fill = cut(.data$value, breaks))) +
    ggplot2::geom_path(
      data = counties,
      ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      alpha = 0.2,
      color = 'grey12'
    ) +
    ggplot2::geom_polygon(
      data = states,
      fill = 'NA',
      color = 'black',
      ggplot2::aes(y = .data$lat, x = .data$long, group = .data$group)
    ) +
    ggplot2::scale_fill_brewer(
      na.value = NA,
      palette = palette,
      direction = direction
    ) +
    ggplot2::labs(
      title = title,
      subtitle = timeString,
      x = 'Longitude', y = 'Latitude', fill = 'PM2.5'
    ) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim, ratio = 4/3) +
    ggplot2::theme_classic() +
    # Customizations to the classic theme
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = "NA", colour = "black")
    )

  return(gg)

}

