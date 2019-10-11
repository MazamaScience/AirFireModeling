# #' @keywords bluesky, ws_event
# #' @export
# #' @title Load Bluesky Model Events into a Single ws_event Object
# #' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
# #' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
# #' @param dailyOutputDir BlueSky web directory
# #' @param model The model of the bluesky output. 
# #' @param subDir subdirectory path containing events data
# #' @param baseUrl base URL for BlueSky output
# #' @description Collects bluesky event data from each day between the startdate and enddate and
# #' returns them as a ws_event list. 
# #' User can pick the model, starting date and end date.
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky/')
# #' bsEvents <- bluesky_loadEvents(20160910, 20160917)
# #' eventMap(bsEvents, datestamp = 3)
# #' }


# bluesky_loadEvents <- function(startdate=NULL,
#                                enddate=NULL,
#                                dailyOutputDir='standard',
#                                model="PNW-1.33km",
#                                subDir='combined',
#                                baseUrl="https://smoke.airfire.org/bluesky-daily/output") {
  
#   # ----- Format and run Sanity Checks -----------------------------------------------------
#   startdate <- parseDatetime(startdate, timezone="UTC")
#   enddate <- parseDatetime(enddate, timezone="UTC")
  
#   # Sanity check
#   if (!model %in% c('PNW-1.33km',
#                     'PNW-4km',
#                     'CANSAC-2km',
#                     'CANSAC-6km',
#                     'NAM36-0.08deg',
#                     'NAM84-0.08deg',
#                     'NAM84-0.15deg')) {
#     stop(paste0("bluesky_aggregate: Model '",model,"' is not recognized.")) 
#   }
  
#   if( startdate > enddate ) {
#     stop(paste0("Cannot have the startdate date (", startdate, ") be before then the enddate date (", enddate , "). 
#                 Are the two dates switched?"))
#   }
  
#   # ---- Download and return Bluesky Events -----------------------------------------
  
#   # Create an empty list for model data
#   bsEventList <- list()
  
#   # Create a numeric vector with each modelRun date.
#   modelRunTimes <- as.character(seq(startdate, enddate, by="1 day"))
  
#   # Download the model runs. If the model doesn't exist then skip to the next one. 
#   for (modelRunTime in modelRunTimes) {
#     modelRun <- strftime(modelRunTime, "%Y%m%d", tz="UTC")
    
#     # Try and load the bluesky_event and put it into out bsEventList. If this doesn't work then let the user know. 
#     result <- try( bsEventList[[modelRun]] <- bluesky_downloadEvents(dailyOutputDir=dailyOutputDir,
#                                                                      model=model,
#                                                                      modelRun=modelRun,
#                                                                      subDir=subDir,
#                                                                      baseUrl=baseUrl)[[1]],
#                    silent=TRUE)
#     if ( class(result)[1] == 'try-error' ) {
#       ###print(paste0('No events for modelRun ', modelRun, '. Skipping to the next modelRun.'))
#       next
#     }
#   }
  
#   return(structure(bsEventList, class=c("ws_event", "list")))
  
# }

