#' @keywords event
#' @import maps
#' @export
#' @title Create Map of Fire Events
#' @param ws_event data list of class \code{ws_event}
#' @param datestamp desired year, month and day (integer or character representing YYYYMM) or index into the ws_event list
#' @param colorBy name of the variable used to create relative colors. colorBy='DATESTAMP' will create a timestep map.
#' @param sizeBy name of the variable used to create relative sizing
#' @param colors a set of colors for different levels of air quality data determined by \code{breaks}'
#' @param breaks set of breaks used to assign colors or a single integer used to provide quantile based breaks - Must also specify the colorBy paramater
#' @param add logical specifying whether to add points to an existing map
#' @param ... additional arguments passed to points() such as 'col' or 'pch'
#' @description Event locations are added to a map and can be sized and colored based on
#' any column found in an event dataframe. All arguments specified in '...' are passed
#' on to the points() function.
#'
#' If \code{add=FALSE}, a basemap is created.
#' @note Because of partial mapping, if you want to specify a color with \code{col=},
#' you must also specify \code{colorBy=NULL}.
#'
#' Using a single number for the \code{breaks} argument will cause the algorithm to use
#' quantiles to determine breaks. This is reasonable behavior when coloring by size, or
#' fire radiative power. In the case of coloring by 'datetime' or some other
#' attribute where you need linear breaks you will have to create those breaks
#' manually and pass them into the breaks argument.
#' @examples
#' \dontrun{
#' setModelDataDir("~/Data/Bluesky")
#'
#' events <- bluesky_loadEvents(startdate=2016091200, enddate=2016091800)
#'
#' # Bounding box for Washington state (also includes strips of OR and ID)
#' xlim <- c(-124.73461, -116.87883)
#' ylim <- c(45.56631, 48.99251)
#' WA_events <- event_subset(events, xlim=xlim, ylim=ylim)
#'
#' eventMap(WA_events, sizeBy='area', colorBy='pm25', lwd=2)
#'
#' # We can also do a timestep map:
#' eventMap(WA_events, sizeBy='area', colorBy='DATESTAMP', lwd=2)
#' }

eventMap <- function(ws_event,
                     datestamp=1,
                     colorBy="pm25",
                     sizeBy=NULL,
                     breaks=5,
                     colors=colorRampPalette(c('orange','red','firebrick'))(breaks),
                     add=FALSE,
                     ...) {
  
  # NOTE:  Datestamp could be either YYYYMMDD or an integer index
  datestamp <- as.integer(datestamp)
  if ( as.integer(datestamp) < 1e6 ) {
    if ( datestamp > length(ws_event) ) {
      stop(paste0("ws_event does not contain datestamp=",datestamp," separate days"))
    }
    datestamp <- names(ws_event)[[datestamp]]
  } else {
    datestamp <- as.character(datestamp)
  }
  
  # Sanity check datestamp
  if ( !datestamp %in% names(ws_event) ) {
    stop(paste0("ws_event does not contain datestamp=", datestamp))
  }
  
  if ( colorBy != "DATESTAMP" &&
       !is.null(colorBy) &&
       !(colorBy %in% colors()) &&
       !(colorBy %in% names(ws_event[[1]])) ) {
    stop("Error: The colorBy paramater must either be 'DATESTAMP', NULL,
         a color (e.g. 'red') or an event column name (e.g. 'pm25').")
  }
  
  ############################
  ############################
  ############################
  
  # Were going to use this to store each day's events.
  dataList <- list()

  # If we are doing a timestep map, put all the events in the dataList and make a vector of colors for each day
  # If we don't do a timestep map then our dataList will only have one dataframe of events, specified with the datestamp param.
  # Using any() because colorBy == "DATESTAMP" returns a logical vector, this could be a bug.
  if( !is.null(colorBy) && colorBy == "DATESTAMP" ) {
    paletteFunc = colorRampPalette(c('gray70', 'yellow','firebrick'))
    dataList <- ws_event
    colors <- paletteFunc(length(dataList))
    if ( !add ) {
      xlim = c(180, -180)
      ylim = c(90, -90)
      for (day in names(dataList)) {
        xlim_new <- adjustRange(range(dataList[[day]]$longitude), 1.5, 0.5)
        ylim_new <- adjustRange(range(dataList[[day]]$latitude), 1.5, 0.5)
        xlim[1] <- min(xlim[1], xlim_new[1], na.rm = TRUE)
        xlim[2] <- max(xlim[2], xlim_new[2], na.rm = TRUE)
        ylim[1] <- min(ylim[1], ylim_new[1], na.rm = TRUE)
        ylim[2] <- max(ylim[2], ylim_new[2], na.rm = TRUE)
      }
    }
  } else {
    dataList[[datestamp]] <- ws_event[[datestamp]]
    if ( !add ) {
      xlim <- adjustRange(range(dataList[[datestamp]]$longitude), 1.5, 0.5)
      ylim <- adjustRange(range(dataList[[datestamp]]$latitude), 1.5, 0.5)
    }
  }

  # Flag to plot base map
  basemapPlotted <- FALSE


  # Set default graphical parameters unless they are passed in
  argsList <- list(...)
  argsList$pch <- ifelse('pch' %in% names(argsList), argsList$pch, 2)
  argsList$cex <- ifelse('cex' %in% names(argsList), argsList$cex, 2)
  # We need a default cex to use when we want to use sizeBy and as well create a TIMESTEP map.
  defaultCex <- argsList$cex

  # We're going to go thru each day of events from the dataList and plot them seperately.
  # If this is a TIMESTEP map then we will go through and plot each day with a different color.
  # If this isn't a TIMESTEP map then we will only have one day in the dataList and this loop will be run once.
  for (day in names(dataList) ) {

    df <- dataList[[day]]
    indexOfDay <- which(names(dataList) == day)

    # If there is no data then don't plot.
    if (nrow(df) == 0) {
      next
    }
    
    # If there hasn't been a base map plotted, plot it
    if ( !basemapPlotted ) {
      # Plot the base map if needed
      if ( !add ) {
        # NOTE:  We should probably always show counties. No need for this to be optional.
        showCounties <- TRUE
        if (showCounties) maps::map('county', xlim=xlim, ylim=ylim, col='gray90')
        maps::map("state", add=TRUE)
      }
      basemapPlotted <- TRUE
    }

    # Sizing by one variable
    if ( !is.null(sizeBy) ) {
      argsList$cex <- defaultCex * df[[sizeBy]] / max(df[[sizeBy]], na.rm = TRUE)
    }

    # This chunk of code figures out colors for the map.
    if ( !is.null(colorBy) ) {
      # If it's a timestep map then pick the color for the day and stick it in argsList
      if ( colorBy == "DATESTAMP" ) {

        dayCount <- length(dataList)
        argsList$col <- grDevices::adjustcolor(colors[indexOfDay], 0.5)
      # If the colorBy is an actual color then stick that color in argsList
      # NOTE: This could be because using the param col matches to colorBy.
      # Do not use %in% colors() becuase it won't be true if the opacity isn't 1
      } else if ( stringr::str_length(colorBy) == 9 &&
                  stringr::str_sub(colorBy, 1, 1) == '#' &&
                  !colorBy %in% names(df) ) {
        argsList$col <- colorBy
      # If colorBy is a name for a column in df (maybe pm25) then color by that value using breaks
      } else if ( colorBy %in% names(df) ) {
        if ( length(breaks) == 1 ) {
          probs <- seq(0,1,length.out=(breaks+1))
          breaks <- stats::quantile(df[[colorBy]], probs=probs, na.rm=TRUE)
        }
        indices <- .bincode(df[[colorBy]], breaks=breaks, include.lowest=TRUE)
        # colors <- paletteFunc(max(indices,na.rm=TRUE))
        argsList$col <- colors[indices]
      }
    }

    # Assign the x and y arguments
    argsList$x <- df$longitude
    argsList$y <- df$latitude

    # Call the points() function
    do.call(points, argsList)

  }

}
