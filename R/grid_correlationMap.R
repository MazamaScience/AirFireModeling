#' @keywords bs_grid, ws_monitor
#' @export
#' @title Correlation Map
#' @param bs_grid bs_grid data list
#' @param ws_monitor monitor data list
#' @param monitorID monitor ID for correlation to model data
#' @param param data parameter to plot
#' @param xlim longitude range to be plotted
#' @param ylim latitude range to be plotted
#' @param tlim time range tover which to calculation the correlation
#' @param breaks used by \code{image()} function to partition correlation values into different colors
#' @param colors vector of colors to use
#' @param zoom range extension as a fraction of the coordinate range
#' @param plotter either `base` or `ggplot`
#' @description Plots a map of the correlation of bluesky model data with a monitoring timeseries.
#'
#' When \code{colors=NULL}, a color palette is given by \code{colorRampPalette}.
#' Note: when using `plotter = ggplot`, zoom, colors and breaks are invalidated.
#' @seealso \link{grid_map}
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' ws_monitor <- monitor_load(20191014, 20191016) %>%
#'   monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")
#' bs_grid <- bluesky_load(model = 'CANSAC-1.33km', modelRun = 20191014)
#' grid_correlationMap(bs_grid, ws_monitor)
#' }
grid_correlationMap <- function(
  bs_grid,
  ws_monitor,
  monitorID = NULL,
  param = 'pm25',
  xlim = NULL,
  ylim = NULL,
  tlim = NULL,
  breaks = NULL,
  colors = NULL,
  zoom = 1.5,
  plotter = 'base'
) {

  if ( !"bs_grid" %in% class(bs_grid) ) {
    stop("bs_grid object is not of class 'bs_grid'.")
  }

  if ( !"ws_monitor" %in% class(ws_monitor) ) {
    stop("ws_monitor object is not of class 'ws_monitor'.")
  }

  if ( !(length(dim(bs_grid$data[[1]])) == 3) ) {
    stop("Currently, only 3-D grids are supported.")
  }

  if ( ncol(ws_monitor$data) > 2 ) {
    if ( is.null(monitorID) ) {
      ws_name <- deparse(substitute(ws_monitor))
      stop(paste0("ws_monitor may only contain data from a single monitor. Please provide a monitorID from ", ws_name ,"$meta$monitorID"))
    } else {
      if ( monitorID %in% ws_monitor$meta$monitorID ) {
        ws_monitor <- monitor_subset(ws_monitor, monitorIDs = monitorID)
      } else {
        stop(paste0("monitorID ",monitorID," not found in ws_monitor meta data"))
      }
    }
  } else {
    monitorID <- ws_monitor$meta$monitorID
  }

  if ( is.null(breaks) ) {
    breaks <- c(-1,-.7,-.5,-.3,-.1,.1,.3,.5,.7,1)
  }
  if ( is.null(colors) ) {
    colors <- rev(RColorBrewer::brewer.pal(9,'RdBu'))
  }

  # tlim handling
  if ( !is.null(tlim) ) {
    tlim <- as.character(tlim)
    if ( nchar(tlim)[1] == 8 ) {
      tlim[1] <- paste0(tlim[1],"00")
    }
    if ( nchar(tlim)[2] == 8 ) {
      tlim[2] <- paste0(tlim[2],"23")
    }
    tlim <- parseDatetime(tlim)
  }

  tlimBeg <- max(bs_grid$time[1], ws_monitor$data$datetime[1], tlim[1])
  tlimEnd <- min(bs_grid$time[length(bs_grid$time)], ws_monitor$data$datetime[length(ws_monitor$data$datetime)], tlim[2])
  tlim <- c(tlimBeg,tlimEnd)

  # set xlim
  if ( length(xlim) == 2 ) {
    xlim <- adjustRange(xlim, zoom)
  } else {
    if ( is.null(xlim) ) {
      xmid <- ws_monitor$meta$longitude
    } else {
      xmid <- xlim
    }
    xdiff <- 1/zoom
    xlim <- c( max(range(bs_grid$longitude)[1], xmid-4*xdiff),
               min(range(bs_grid$longitude)[2], xmid+4*xdiff) )
  }

  # set ylim
  if ( length(ylim) == 2 ) {
    ylim <- adjustRange(ylim, zoom)
  } else {
    if ( is.null(ylim) ) {
      ymid <- ws_monitor$meta$latitude
    } else {
      ymid <- ylim
    }
    ydiff <- 1/zoom
    ylim <- c( max(range(bs_grid$latitude)[1], ymid-4*ydiff),
               min(range(bs_grid$latitude)[2], ymid+4*ydiff) )
  }

  # subset bs_grid if necessary
  bs_gridSub <- grid_subset(bs_grid, xlim, ylim, tlim)
  lon <- bs_gridSub$longitude
  lat <- bs_gridSub$latitude
  gridData <- bs_gridSub$data[[param]][,,]

  # Subset ws_monitor to have the same times as bs_grid
  ws_monSub <- monitor_subset(ws_monitor, tlim = tlim)
  monitorData <- ws_monSub$data[,2]

  # Calculation the correlation
  # NOTE:  Suppress warnings to avoid "standard deviation is zero" for model grid cells with no smoke
  gridSlice <- suppressWarnings( apply(gridData, c(1,2), stats::cor, monitorData, "complete.obs") )

  # ----- Mapping ------

  if ( grepl('gg', plotter) ) {

    # create ggmap
    states <- ggplot2::map_data('state', xlim = xlim, ylim = ylim)
    counties <- ggplot2::map_data('county', xlim = xlim, ylim = ylim)

    gs <- t(gridSlice)[nrow(t(gridSlice)):1,] # flip coords

    ras <- raster::raster(gs, xmn = xlim[1], xmx = xlim[2], ymn = ylim[1], ymx = ylim[2])

    gg <- rasterVis::gplot(ras) +
      ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::geom_path(data = counties, ggplot2::aes(x = long, y = lat, group = group), alpha = 0.2, color = 'grey12') +
      ggplot2::geom_polygon(data = states, ggplot2::aes(y = lat, x = long, group = group), fill = 'NA', color = 'black') +
      ggplot2::scale_fill_gradient2(na.value = 'NA', low = 'dodgerblue', mid = 'white', high = 'firebrick') +
      ggplot2::geom_point(ggplot2::aes(x = ws_monSub$meta$longitude, y = ws_monSub$meta$latitude),
                          shape = 23, colour = 'black', fill = 'NA', size = 2, stroke = 1) +
      ggplot2::labs(title = paste0("Correlation of ", monitorID," vs. ", bs_grid$model),
                    subtitle = paste0(tlim[1], " to ",tlim[2]),
                    x = 'Longitude', y = 'Latitude', fill = 'Correlation') +
      ggplot2::theme_classic() +
      ggplot2::coord_fixed(xlim = xlim, ylim = ylim,ratio = 4/3)

    return(gg)

  } else {

    # style options
    # create default palette if none is passed in
    if (is.null(colors)) {
      palette <- colorRampPalette(c("white", "orange", "firebrick"))
      colors <- palette(length(breaks)-1)
    }

    # create base map
    maps::map('state', xlim = xlim, ylim = ylim)
    title(main=paste0("Correlation of ", monitorID," vs. ", bs_grid$model, "\n", tlim[1], " to ",tlim[2]))
    image(lon, lat, gridSlice, breaks = breaks, col = colors, add = TRUE)
    maps::map('county', col = 'gray80', xlim = xlim, ylim = ylim, add = TRUE)
    maps::map('state', xlim = xlim, ylim = ylim, add = TRUE)
    maps::map.axes()

    labels <- rev(paste0(lag(breaks),' to ',breaks)[-1])
    legend("bottomright", col=rev(colors), legend=labels, pch=16, cex=.7)

    addBullseye(ws_monSub$meta$longitude,ws_monSub$meta$latitude)

  }
  if ( FALSE ) {
    ws_monitor <-
      monitor_load(20191014, 20191016) %>%
      monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")
    bs_grid <- bluesky_load(model = 'CANSAC-1.33km',modelRun = 20191014)
    param <- 'pm25'
    monitorID=NULL
    param="pm25"
    xlim=NULL
    ylim=NULL
    tlim=NULL
    breaks=c(-1,-.7,-.5,-.3,-.1,.1,.3,.5,.7,1)
    colors=rev(RColorBrewer::brewer.pal(9,'RdBu'))
    zoom=1.5
  }

}
