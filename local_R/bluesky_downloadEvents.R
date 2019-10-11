# #' @keywords internal, bluesky, ws_event
# #' @export
# #' @title Download Bluesky Model Events Data from PWFSL
# #' @param dailyOutputDir BlueSky web directory
# #' @param model The model of the bluesky output. 
# #' @param modelRun The model run to load data from. 
# #' @param subDir subdirectory path containing events data
# #' @param baseUrl base URL for BlueSky output
# #' @description Downloads bluesky event data for a single model run and 
# #' parses it into a \emph{ws_events} object.
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky/')
# #' 
# #' # Load all events from September 09, 2016 and map 
# #' # the events that happened on the third day (August 10th)
# #' bsEvent <- bluesky_downloadEvents(modelRun=20160912, 
# #'                                   baseUrl='https://smoke.airfire.org/bluesky-daily/output')
# #' eventLeaflet(bsEvent)
# #' }

# bluesky_downloadEvents <- function(dailyOutputDir="standard",
#                                    model="PNW-1.33km",
#                                    modelRun,
#                                    subDir="combined",
#                                    baseUrl="https://smoke.airfire.org/bluesky-daily/output") {
  
#   # Use latest date if modelRun is not specified
#   if ( is.null(modelRun) ) {
#     stop("Required parameter 'modelRun' is missing")
#   } else {
#     modelRun <- as.character(modelRun)
#   }
  
#   # Turn the modelRun into a date time, then format it into YYYYMMDD00 format
#   modelRun <- paste0(strftime(parseDatetime(modelRun, timezone="UTC"), "%Y%m%d", tz="UTC"), "00")
  
#   # Not sure why this check is here but it doesn't hurt.
#   if ( length(model) > 1 || length(modelRun) > 1 ) {
#     warning("'model' or 'modelRun' parameter has multiple values -- first value being used.")
#     model <- model[1]
#     modelRun <- modelRun[1]  
#   }
  
#   # Create the URLs and local name
#   if ( is.null(subDir) ) {
#     fire_locations_csv <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/data/fire_locations.csv")
#     fire_events_csv <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/data/fire_events.csv")
#   } else {
#     fire_locations_csv <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/", subDir, "/data/fire_locations.csv")
#     fire_events_csv <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/", subDir, "/data/fire_events.csv")
#   }
  
#   # Read in the raw data files
#   locations <- suppressMessages( readr::read_csv(fire_locations_csv) )
#   events <- suppressMessages( readr::read_csv(fire_events_csv) )
  
#   if ( nrow(events) == 0 ) {
#     stop(paste0('No events for modelRun ',modelRun))
#   }
  
#   # modify the event$id name so we can "join"
#   names(events)[1] <- 'event_id'
  
#   df <- dplyr::left_join(locations, events, by="event_id")
  
#   # Create a YYYYMMDD 'datestamp' column and a POSIXct 'datetime'
#   # NOTE:  'date_time' currently contains something like "201509090000-07:00" (GMT datestamp plus GMT offset)
#   df$datestamp <- stringr::str_sub(df$date_time,1,8)
#   df$datetime <- lubridate::ymd(df$datestamp)
  
#   # Create a list of daily events dataframes
#   ws_event <- list()
  
#   # Because each date is identical just grab the one associated with the passed in modelRun.
#   modelRun <- stringr::str_sub(modelRun,1,8)
#   ws_event[[modelRun]] <- df[df$datestamp == modelRun,]
  
#   return(structure(ws_event, class = c("ws_event", "list")))
  
# }


