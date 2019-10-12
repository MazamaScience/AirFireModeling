#' @keywords ws_event
#' @export
#' @title Subset Event Data
#' @param ws_event \emph{ws_event} object
#' @param filter a filter to use on the ws_event object
#' @return A emph{ws_event} object with a subset of the input ws_event object.
#' @description The incoming ws_monitor object is filtered according to \code{filter}.
#'
#' A filter is any valid R expression appropriate in the context of the dataframe containing event data.
#' Typically this will be something you might use to create a logical mask like \code{pm25 > 0.2}
#' or \code{pm25 > 0.2 & area > 100}.
#' @examples \dontrun{
#' events <- bluesky_loadEvents(20160910, 20160924)
#' events <- event_subsetBy(events, pm25 > 0.2)
#' map('county',c('washington','oregon','idaho'),col='gray90')
#' map('state',c('washington','oregon','idaho'), add=TRUE)
#' eventMap(events, sizeBy='area', colorBy='pm25', lwd=2, add=TRUE)
#' }

event_subsetBy <- function(ws_event, filter) {

  # Sanity check
  if (!"ws_event" %in% class(ws_event)) {
    stop("ws_event object is not of class 'ws_event'.")
  }

  # Create a condition call, basically an expression that isn't run yet.
  # http://adv-r.had.co.nz/Computing-on-the-language.html#nse <<< More information on calls
  condition_call <- substitute(filter)

  # Loop through all remaining dataframes, subsetting as instructed.
  for ( i in seq(length(ws_event)) ) {
    df <- ws_event[[i]]

    # It is very easy to pass in a filter which wont match an columns in our df so lets catch any of those
    result <- try(conditionMask <- eval(condition_call, df),
                  silent = FALSE)

    # Handle errors
    if (class(result)[1] == 'try-error') {
      err_msg <- geterrmessage()
      if (stringr::str_detect(err_msg, 'not found')) {
        stop(paste0("Bad filter: \"", deparse(condition_call), "\" does not match any dataframe column."))
      } else {
        stop(err_msg)
      }
    }

    # Save the dataframe with all the masks applied to it
    ws_event[[i]] <- df[conditionMask,]
  }

  return(ws_event)

}

