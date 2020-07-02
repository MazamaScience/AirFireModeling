#' @export
#' @title Geographic map of BlueSky model output
#'
#' @description Creates a \pkg{ggplot2} plot object from a Raster\*
#' object. The returned plot object can be plotted or can be enhanced with
#' additional \pkg{ggplot2} intructions.
#'
#' @details If a list of Raster\* objects is provided, a small-multiples
#' plot is created.
#'
#' The \code{index} is typically associated with a time-axis or \code{RasterLayer},
#' e.g \code{index = 1} is the first hour of a model.
#'
#' @param raster A Raster\* object or a list of Raster\* objects.
#' @param index And index into the Raster\* object. See details.
#' @param palette The color palette used to map cell values. This must be one
#' of the palettes available through \code{ggplot2::scale_colour_brewer()}.
#' @param breaks The breaks used to map cell values to colors.
#' @param direction Numeric. \code{direction = -1} reverses color palette.
#' @param title (Optional) A plot title.
#' @param timezone Olson timezone in which times will be displayed.
#' @param col_state Color of state lines. (use \code{'transparent'} to hide them.)
#' @param col_county Color of county lines. (use \code{'transparent'} to hide them.)
#' @param verbose Logical to display messages.
#'
#' @return A ggplot object.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   model = "PNW-4km",
#'   modelRun = c(2019100800, 2019100900, 2019101000, 2019101100),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_ggmap(rasterList, title="PNW-4km", index = 3)
#' }
raster_ggmap <- function(
  raster,
  index = 1,
  palette = 'Greys',
  breaks = c(0, 12, 35, 55, 150, 250, 350, Inf),
  direction = 1,
  title = 'PM2.5',
  timezone = 'UTC',
  col_state = 'black',
  col_county = 'gray80',
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)

  if ( !is.list(raster) && !raster_isRaster(raster) )
    stop("Parameter 'raster' must be a single or a list of Raster* objects.")

  if ( !is.numeric(index) || length(index) > 1 ) {
    stop(paste0(
      "Parameter 'index' must be a single numeric value.  ",
      "Use raster_facet() to plot multiple timesteps.")
    )
  }

  availablePalettes <- rownames(RColorBrewer::brewer.pal.info)
  if ( !palette %in% availablePalettes )
    stop(sprintf("'%s' is not a recognized palette. Please see ?ggplot2::scale_colour_brewer."))

  if ( !is.numeric(breaks) )
    stop("Parameter 'breaks' must be a numeric vector.")

  if ( !direction %in% c(-1,1) )
    stop("Parameter 'direction' must be either 1 or -1.")

  if ( !is.character(title) )
    stop("Parameter 'title' must be character.")

  if ( !timezone %in% OlsonNames())
    stop("Parameter 'timezone' is not recognized. Please see ?OlsonNames/.")

  # ----- Dispatch method ------------------------------------------------------

  UseMethod('raster_ggmap', raster)

}

# ===== Method Dispatch ========================================================

# NOTE:  For an explanation of S3 method dispatch, see:
# NOTE:    http://adv-r.had.co.nz/OO-essentials.html#s3

#' @describeIn raster_ggmap Method for Raster* objects.
#' @export
raster_ggmap.Raster <- function(
  raster, # a single RasterBrick
  index = 1,
  palette = 'Greys',
  breaks = c(0, 12, 35, 55, 150, 250, 350, Inf),
  direction = 1,
  title = 'PM2.5',
  timezone = 'UTC',
  col_state = 'black',
  col_county = 'gray80',
  verbose = TRUE
) {

  if ( index > dim(raster)[3] )
    stop('Index out of range.')

  # NOTE:  Use list syntax to pull a RasterLayer out of a RasterBrick

  layer <- raster[[index]]
  .plot_map(layer, palette, breaks, direction, title, timezone, col_state, col_county)

}

#' @describeIn raster_ggmap Multi-threaded method for a list of Raster* objects.
#' @export
raster_ggmap.list <- function(
  raster, # a list of RasterBricks
  index = 1,
  palette = 'Greys',
  breaks = c(0, 12, 35, 55, 150, 250, 350, Inf),
  direction = 1,
  title = 'PM2.5',
  timezone = 'UTC',
  col_state = 'black',
  col_county = 'gray80',
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

  layerNames <- names(layerList)
  gg_list <- list()
  for ( i in seq_along(layerList) ) {
    title <- layerNames[i]
    layer <- layerList[[i]]
    gg_list[[title]] <- .plot_map(layer, palette, breaks, direction, title, timezone, col_state, col_county)
  }

  # Assemble individual plots into a grid
  return(cowplot::plot_grid(plotlist = gg_list))

}

# ===== Internal Functions =====================================================

# NOTE: Internal function for plotting an individual ggplot2 raster layer
.plot_map <- function(
  layer,
  palette = 'Greys',
  breaks = c(0, 12, 35, 55, 150, 250, 350, Inf),
  direction = 1,
  title = 'PM2.5',
  timezone = 'UTC',
  col_state = 'black',
  col_county = 'gray80'
) {

  # Parameter validation handled in calling funcitions

  # ----- Prepare data ---------------------------------------------------------

  # Get layer coordinate limits
  limits <- raster::extent(layer)
  xlim <- c(limits@xmin, limits@xmax)
  ylim <- c(limits@ymin, limits@ymax)

  timeString <- raster_createTimeStrings(layer)

  # Get state and counties data for plotting
  states <- ggplot2::map_data('state', xlim = xlim, ylim = ylim)
  counties <- ggplot2::map_data('county', xlim = xlim, ylim = ylim)

  # ----- Create plot ----------------------------------------------------------

  # See: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

  gg <-
    rasterVis::gplot(layer) +
    ggplot2::geom_raster(ggplot2::aes(fill = cut(.data$value, breaks))) +
    ggplot2::geom_path(
      data = counties,
      ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      fill = 'NA',
      color = col_county
    ) +
    ggplot2::geom_polygon(
      data = states,
      ggplot2::aes(y = .data$lat, x = .data$long, group = .data$group),
      fill = 'NA',
      color = col_state
    ) +
    ggplot2::scale_fill_brewer(
      na.value = NA,
      palette = palette,
      direction = direction
    ) +
    ggplot2::labs(
      title = title,
      subtitle = timeString,
      x = 'Longitude',
      y = 'Latitude',
      fill = 'PM2.5'
    ) +
    ggplot2::coord_fixed(ratio = 1.3, xlim = xlim, ylim = ylim) +
    ggplot2::theme_classic() +
    # Customizations to the classic theme
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = "NA", colour = "black")
    )

  return(gg)

}

