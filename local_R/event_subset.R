#' @keywords ws_event
#' @export
#' @title Create a Subset of a ws_event Object
#' @param ws_event ws_event object
#' @param xlim optional vector with low and high longitude limits
#' @param ylim optional vector with low and high latitude limits
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH] or \code{POSIXct})
#' @param dropDates flag specifying whether to remove dates with no remaining events
#' @description The incoming \code{ws_event} is filtered according
#' to the parameters passed in.  If any parameter is not specified, that parameter will not be used
#' in the filtering.
#' @return A \emph{ws_event} object with a subset of \code{ws_event}.
#' @examples \dontrun{
#' events <- bluesky_loadEvents(20160910, 20160924)
#'
#' # Bounding box for Washington state (also includes strips of OR and ID)
#' xlim <- c(-124.73461, -116.87883)
#' ylim <- c(45.56631, 48.99251)
#' wa_events <- event_subset(events, xlim=xlim, ylim=ylim)
#'
#' eventMap(wa_events, sizeBy='area', colorBy='DATESTAMP', lwd=2)
#' }

event_subset <- function(ws_event, xlim=NULL, ylim=NULL, tlim=NULL,
                         dropDates=TRUE) {
  
  # Sanity check
  if ( !"ws_event" %in% class(ws_event) ) {
    stop("ws_event object is not of class 'ws_event'.")
  }
  
  # Remove any dataframes outside of tlim
  if ( !is.null(tlim) ) {
    
    if (class(tlim)[1] == 'numeric' || class(tlim)[1] == 'character') {
      tlim <- parseDatetime(tlim) # Don't apply timezone #, timezone=timezone)
    } else if ( "POSIXct" %in% class(tlim) ) {
      # leave tlim alone
    } else {
      stop(paste0("Invalid argument type: class(tlim) = '",paste0(class(tlim),collapse=" "),"'"))
    }
    
    for ( datestamp in names(ws_event) ) {
      datetime <- parseDatetime(datestamp)
      if ( datetime < tlim[1] | datetime > tlim[2] ) {
        ws_event[[datestamp]] <- NULL
      }
    }
    
  }
  
  # NOTE:  It might be more efficient to combine all data into a single tibble but
  # NOTE:  for now (2017-03-23) we will keep this list-of-dataframe structure as it
  # NOTE:  matches the list-of-SPDF used for ws_polygon objects.
  # Loop through all remaining dataframes, subsetting as instructed
  for ( i in seq(length(ws_event)) ) {
    
    df <- ws_event[[i]]
    
    # Create a default mask to will not change the df
    defaultMask <- rep(TRUE, nrow(df))
    
    # If an xlim, ylim wasn't passed in then use the default mask
    # NOTE:  ifelse doesn't work here as it collapses a vector into a single value
    lonMask <- if (is.null(xlim)) defaultMask else df$longitude >= xlim[1] & df$longitude <= xlim[2] 
    latMask <- if (is.null(ylim)) defaultMask else df$latitude >= ylim[1] & df$latitude <= ylim[2]
    
    # Save the dataframe with all the masks applied to it
    ws_event[[i]] <- df[lonMask & latMask,]
    
  }
  
  if ( dropDates ) {
    for ( datestamp in names(ws_event) ) {
      if ( nrow(ws_event[[datestamp]]) == 0 ) {
        ws_event[[datestamp]] <- NULL
      }
    }
  }
  
  return(ws_event)
  
}

