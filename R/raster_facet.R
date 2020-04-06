#' @export
#' @title Small multiples map of BlueSky model output
#'
#' @param raster A Raster\* object.
#' @param index Vector of indices used to subset \code{raster}
#' @param palette Color palette used to map cell values. This must be either
#' \code{"aqi"} or one of the palette names available through
#' \code{ggplot2::scale_colour_brewer()}.
#' @param breaks The breaks used to map cell values to colors. (Unless
#' \code{palette = "aqi"} in which case AQI breaks are used.)
#' @param direction Numeric. \code{direction = -1} reverses color palette.
#' @param title (Optional) A plot title.
#' @param timezone Olson timezone in which times will be displayed.
#' @param ncol Number of columns in the output plot.
#' @param col_state Color of state lines. (use \code{'transparent'} to hide them.)
#' @param col_county Color of county lines. (use \code{'transparent'} to hide them.)
#'
#' @description This function creates a grid of plots, each displaying a small
#' map of a single hour from the model output passed in as \code{raster}. By
#' default, all hours are plotted. The \code{index} parameter can be used to
#' subset the hours.
#'
#' @return A gg object.
#'
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   model = "PNW-4km",
#'   modelRun = c(2019100900),
#'   xlim = c(-125, -117),
#'   ylim = c(42, 47)
#' )
#'
#' # First 24 hours
#' raster_facet(
#'   rasterList[[1]],
#'   index = 1:24,
#'   col_county = 'transparent'
#' )
#'
#' # Fancy!!
#' raster_facet(
#'   rasterList[[1]],
#'   index = seq(3,72,12),
#'   title = "PNW-4km -- run 2019100900",
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Kincade fire
#' rasterList <- raster_load(
#'   model = "CANSAC-4km",
#'   modelRun = c(2019102700),
#'   xlim = c(-124, -121.5),
#'   ylim = c(37.5, 39)
#' )
#'
#' raster_facet(
#'   rasterList[[1]],
#'   title = "Kincade Fire",
#'   ncol = 12
#' )
#'
#' }
#'
raster_facet <- function(
  raster,
  index = NULL,
  palette = 'Greys',
  breaks = c(0,12, 35, 55, 150, 250, 350, Inf),
  direction = 1,
  title = "",
  timezone = 'UTC',
  ncol = NULL,
  col_state = 'black',
  col_county = 'gray80'
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)

  if ( !raster_isRaster(raster) )
    stop("Parameter 'raster' must be a single Raster* object.")

  if ( !is.null(index) ) {
    if ( !is.numeric(index) )
      stop("Parameter 'index' must be numeric.")
  }

  brewerPalettes <- rownames(RColorBrewer::brewer.pal.info)
  otherPalettes <- c("viridis_A")
  availablePalettes <- union(brewerPalettes, otherPalettes)
  if ( !palette %in% availablePalettes )
    stop(sprintf("'%s' is not a recognized palette. Please see ?ggplot2::scale_colour_brewer.", palette))

  if ( !is.numeric(breaks) )
    stop("Parameter 'breaks' must be a numeric vector.")

  if ( !direction %in% c(-1,1) )
    stop("Parameter 'direction' must be either 1 or -1.")

  if ( !is.character(title) )
    stop("Parameter 'title' must be character.")

  if ( !timezone %in% OlsonNames())
    stop("Parameter 'timezone' is not recognized. Please see ?OlsonNames.")

  if ( !is.null(ncol) && !is.numeric(ncol) )
    stop("Parameter 'ncol' must be numeric.")

  # ----- Prepare data ---------------------------------------------------------

  # Get layer coordinate limits
  limits <- raster::extent(raster)
  xlim <- c(limits@xmin, limits@xmax)
  ylim <- c(limits@ymin, limits@ymax)

  # Get state and counties data for plotting
  states <- ggplot2::map_data('state', xlim = xlim, ylim = ylim)
  counties <- ggplot2::map_data('county', xlim = xlim, ylim = ylim)

  # Subset rasterBrick if requested
  if ( !is.null(index) ) {
    if ( raster::nlayers(raster) > 1 )
      raster <- raster::subset(raster, subset = index, drop = FALSE)
  }

  # Determine color scaler
  # TODO:  Get AQI coloring to work
  # if ( palette == 'aqi') {
  #   breaks <- PWFSLSmoke::AQI$breaks_24
  #   scale_colors <- ggplot2::scale_fill_manual(
  #     palette = PWFSLSmoke::aqiPalette()
  #   )
  if ( palette == 'viridis_A' ) {
    scale_colors <-  ggplot2::scale_fill_viridis_d(
      option = 'A',
      direction = direction
    )
  } else {
    scale_colors <- ggplot2::scale_fill_brewer(
      na.value = 'transparent',
      palette = palette,
      direction = direction
    )
  }

  # TODO:  Calcualte appropriate aspect ratio
  # Common for a ful US map
  aspectRatio <- 1.3

  if ( timezone %in% c("UTC", "GMT") ) {
    # Shorthand timestamp when there are lots of plots
    labellerFUN <- .createTimeStamps
  } else {
    # Human friendly when they ask for it (though it may not always fit)
    labellerFUN <- .createTimeStrings
    # NOTE:  Fancy R monkey-patch to modify the default behavior of a function
    formals(labellerFUN) <- list(layerName = "", timezone = timezone, prefix = "")
  }

  # ----- Create plot ----------------------------------------------------------

  # See: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

  gg <-
    rasterVis::gplot(raster) +
    ggplot2::geom_raster(ggplot2::aes(fill = cut(.data$value, breaks = breaks)))

  if ( col_county != 'transparent' ) {
    gg <- gg +
      ggplot2::geom_polygon(
        data = counties,
        ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
        fill = 'NA',
        color = col_county
      )
  }

  if ( col_state != 'transparent' ) {
    gg <- gg +
      ggplot2::geom_polygon(
        data = states,
        fill = 'NA',
        color = col_state,
        ggplot2::aes(y = .data$lat, x = .data$long, group = .data$group)
      )
  }

  gg <- gg +
    ggplot2::facet_wrap(
      ~.data$variable,
      ncol = ncol,
      labeller = ggplot2::labeller(.default = labellerFUN)
    ) +
    scale_colors +
    ggplot2::labs(
      title = title,
      x = 'Longitude',
      y = 'Latitude',
      fill = 'PM2.5'
    ) +
    ggplot2::coord_fixed(ratio = aspectRatio, xlim = xlim, ylim = ylim) +
    ggplot2::theme_classic()

  return(gg)

}

# ===== Internal Functions =====================================================

# Just like raster_createTimeStamps but gets the name passed in
.createTimeStrings <- function(
  layerName = NULL,
  timezone = "UTC",
  prefix = ""
) {

  layerTime <- raster_createLayerNameTimes(layerName)
  timeString <- paste0(prefix, strftime(layerTime, format = "%Y-%m-%d %H:00 %Z", tz = timezone))

  return(timeString)

}

# Just like raster_createTimeStamps but gets the name passed in
.createTimeStamps <- function(
  layerName = NULL
) {

  layerTime <- raster_createLayerNameTimes(layerName)
  timeString <- strftime(layerTime, format = "%Y%m%d%H", tz = "UTC")

  return(timeString)

}

