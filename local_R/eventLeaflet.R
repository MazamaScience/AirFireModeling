# #' @keywords ws_event
# #' @export
# #' @title Leaflet Interactive Map of Events
# #' @param ws_event data list of class \code{ws_event}
# #' @param datestamp desired year, month and day (integer or character representing YYYYMMDD) or index into the ws_event list
# #' @param colorBy name of the variable used to create relative colors. colorBy='DATESTAMP' will create a timestep map.
# #' @param sizeBy name of the variable used to create relative sizing
# #' @param breaks set of breaks used to assign colors or a single integer used to provide quantile based breaks - Must also specify the colorBy paramater
# #' @param colors set of colors for different levels of air quality data determined by \code{breaks}'
# #' @param labels set of labels to show in the legend
# #' @param radius radius of monitor circles
# #' @param opacity opacity of monitor circles
# #' @param maptype optional name of leaflet ProviderTiles to use, e.g. "terrain"
# #' @param popupInfo column names of the ws_event object dataframe to appear in popups
# #' @param showLegend logical value to toggle the legend
# #' @description This function creates interactive maps that will be displayed in RStudio's 'Viewer' tab.
# #' Individual events are plotted over a map of the United States. A timestep map can be created
# #' by passing in \code{colorBy = 'DATESTAMP'}.
# #' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile" names. Current mappings include:
# #' \enumerate{
# #' \item{"roadmap"}{ -- "OpenStreetMap"}
# #' \item{"satellite"}{ -- "Esri.WorldImagery"}
# #' \item{"terrain"}{ -- ""Esri.WorldTopoMap"}
# #' \item{"toner"}{ -- "Stamen.Toner"}
# #' }
# #' 
# #' If a character string not listed above is provided, it will be used as the underlying map tile if available.
# #' See \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of "provider tiles"
# #' to use as the background map.
# #' @return Initiates the interactive leaflet plot in Rstudio's 'Viewer' tab.
# #' @examples
# #' \dontrun{
# #' events <- bluesky_loadEvents(20160910, 20160924)
# #' xlim <- c(-124.73461, -116.87883)
# #' ylim <- c(45.56631, 48.99251)
# #' WA_events <- event_subset(events, xlim=xlim, ylim=ylim)
# #' eventLeaflet(WA_events, colorBy="DATESTAMP")
# #' }

# eventLeaflet <- function(ws_event,
#                          datestamp=1,
#                          colorBy='pm25',
#                          sizeBy=NULL,
#                          breaks=5,
#                          colors=colorRampPalette(c('orange','red','firebrick'))(breaks),
#                          labels=NULL,
#                          radius=10, opacity=0.7, maptype='terrain',
#                          popupInfo = c('event_name', 'datetime', 'id', 'pm25', 'area'),
#                          showLegend=TRUE) {

#   # NOTE:  Datestamp could be either YYYYMMDD or an integer index
#   datestamp <- as.integer(datestamp)
#   if ( as.integer(datestamp) < 1e6 ) {
#     if ( datestamp > length(ws_event) ) {
#       stop(paste0("ws_event does not contain datestamp=",datestamp," separate days"))
#     }
#     datestamp <- names(ws_event)[[datestamp]]
#   } else {
#     datestamp <- as.character(datestamp)
#   }
  
#   # Sanity check datestamp
#   if ( ! datestamp %in% names(ws_event) ) {
#     stop(paste0("ws_event does not contain datestamp=", datestamp))
#   }
  
#   if ( colorBy != "DATESTAMP" &&
#        !is.null(colorBy) &&
#        !(colorBy %in% colors()) &&
#        !(colorBy %in% names(ws_event[[1]])) ) {
#     stop("Error: The colorBy paramater must either be 'DATESTAMP', NULL,
#              a color (e.g. 'red') or an event column name (e.g. 'pm25').")
#   }
  
#   # If we are doing a timestep map then combine each day's events into one
#   # dataframe. Then set up a color for each event and create labels and 
#   # a title for the legend.
#   if ( colorBy == "DATESTAMP" ) {
#     eventsDF <- do.call(rbind, ws_event)
#     # eventsDF <- dplyr::bind_rows(ws_event)
#     # NOTE: line above commented out for following error:
#     # NOTE: Error in bind_rows_(x, .id) : 
#     # NOTE: Argument 1 must be a data frame or a named atomic vector, not a ws_event/list
#     # NOTE: See GitHub Issue here: https://github.com/tidyverse/dplyr/issues/2962
#     paletteFunc = colorRampPalette(c('gray70', 'yellow','firebrick'))
#     colors <- paletteFunc(length(ws_event))
    
#     eventsDF$col <- colors[factor(eventsDF$datetime)]
#     labels <- levels(factor(eventsDF$datetime))
#     tableTitle <- "Dates"
#   } else {
#     eventsDF <- ws_event[[datestamp]]
#   }
  
#   # This chunk creates a new column called description which is used
#   # to display info when a leaflet circle is clicked.
  
#   # If there are no column names in the popupInfo, then don't make descriptions
#   if ( ! all(popupInfo %in% names(eventsDF)) ) {
#     popupInfo <- NULL
#   }
  
#   # popupInfo -----------------------------------------------------------------
#   if ( !is.null(popupInfo) ) {
    
#     # Create a description for each event depending on the passed in info.
#     descVector <- character()
    
#     for ( rowIndex in 1:nrow(eventsDF) ) {
#       rowDesc <- character()
#       for ( colName in popupInfo ) {
#         if ( !(colName %in% names(eventsDF)) ) {
#           next
#         } else if ( colName == 'event_name' ) {
#           rowDesc <- paste0(eventsDF[[colName]][rowIndex], "<br/>")
#         } else {
#           rowDesc <- paste0(rowDesc, colName, ": ", eventsDF[[colName]][rowIndex], "<br/>")
#         }
#       }
#       descVector[rowIndex] <- rowDesc
#     }
    
#   } else {
    
#     descVector = ''
    
#   }
#   eventsDF$description <- descVector
  
#   # colorBy -------------------------------------------------------------------
#   if ( !is.null(colorBy) ) {
    
#     # If the user picks a normal color then set every event to that color
#     if ( colorBy %in% colors() ) {
      
#       eventsDF$col <- colorBy

#     } else if ( colorBy %in% names(eventsDF) ) {
      
#       # If the user wants to color by a column value, create breaks if needed
#       # and then set each event to a certain color depending on the breaks. Also
#       # set legend title and labels for each break.
      
#       if ( length(breaks) == 1 ) {
#         probs <- seq(0,1,length.out=(breaks+1))
#         breaks <- stats::quantile(eventsDF[[colorBy]], probs=probs, na.rm=TRUE)
#       }
#       indices <- .bincode(eventsDF[[colorBy]], breaks=breaks, include.lowest=TRUE)
#       ###colors <- paletteFunc(max(indices,na.rm=TRUE))
#       eventsDF$col <- colors[indices]
#       if ( is.null(labels) ) {
#         if ( colorBy == "pm25" ) {
#           labels <- round(utils::head(breaks, -1), digits=1) # Exclude the last one
#         } else {
#           labels <- signif(utils::head(breaks, -1), digits=3) # Exclude the last one
#         }
#       }
#       tableTitle <- colorBy
      
#     }
    
#   }
  
#   # NOTE: Should we keep this? This makes some of the events on the map
#   # rather small.
#   if ( !is.null(sizeBy) ) {
#     eventsDF$radius <- 2 * radius * eventsDF[[sizeBy]] / max(eventsDF[[sizeBy]], na.rm = TRUE)
#   } else {
#     eventsDF$radius <- radius
#   }
  
#   # Extract view information
#   lonRange <- range(eventsDF$longitude)
#   latRange <- range(eventsDF$latitude)
#   maxRange <- max(diff(lonRange),diff(latRange))
#   # Determine appropriate zoom level
#   if (maxRange > 20) {
#     zoom <- 4
#   } else if (maxRange > 10) {
#     zoom <- 5
#   } else if (maxRange > 5) {
#     zoom <- 6
#   } else if (maxRange > 2) {
#     zoom <- 7
#   } else if (maxRange > 1) {
#     zoom <- 8
#   } else if (maxRange > 0.5) {
#     zoom <- 9
#   } else if (maxRange > 0.2) {
#     zoom <- 10
#   } else if (maxRange > 0.1) {
#     zoom <- 11
#   } else {
#     zoom <- 12
#   }
  
#   # Convert maptype to a character string that addProviderTiles can read
#   if ( missing(maptype) || maptype == 'terrain') {
#     providerTiles <- "Esri.WorldTopoMap"
#   } else if ( maptype == "roadmap" ) {
#     providerTiles <- "OpenStreetMap"
#   } else if ( maptype == "toner" ) {
#     providerTiles <- "Stamen.Toner"
#   } else if (maptype == "satellite" ) {
#     providerTiles <- "Esri.WorldImagery"
#   } else {
#     providerTiles <- maptype
#   }
  
#   map <- leaflet::leaflet() %>%
#     leaflet::addProviderTiles(providerTiles) %>%
#     leaflet::addCircleMarkers(
#       lng = eventsDF$longitude,
#       lat = eventsDF$latitude,
#       radius=eventsDF$radius,
#       fillColor=eventsDF$col,
#       fillOpacity=opacity,
#       stroke=FALSE,
#       popup=eventsDF$description
#     )
  
#   if (showLegend) {
#     map <- leaflet::addLegend(
#       map = map,
#       position='bottomright',
#       colors=rev(colors), # show low levels at the bottom
#       labels=rev(labels),  # show low levels at the bottom
#       opacity = 1,
#       title=tableTitle)
#   }
  
#   # Display the map
#   print(map)
  
# }

