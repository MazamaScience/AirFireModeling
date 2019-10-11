# #' @keywords bluesky, ws_grid
# #' @export
# #' @title Aggregate Multiple Bluesky Models into a Single ws_grid Object
# #' @param model The model of the bluesky output. 
# #' @param runStart The startdate of the model run. 
# #' Supports either 10 digits format (e.g: 2015072100) or 8 digit format (e.g: 20150721)
# #' @param runEnd The enddate of the model run.
# #' @param subDir subdirectory path containing netcdf data
# #' @param spacing time spacing (hr) between each model run -- overrides default values
# #' @param chunk The portion of the time (of width = 'spacing') to be chosen from each model run. If X = spacing, then 
# #' Chunk = 1 corresponds to the first X hours of the each model run, Chunk = 2 corresponds to the next X hours of each
# #' model run, and so on. See Note below for further explanation.
# #' @param param parameter name
# #' @description Combines multiple bluesky gridded output data and produce a single bluesky object. 
# #' User can pick the starting date, end date, the hourly spacing between
# #' the each model run, and the chunk index for the portion of time to be extracted.
# #' Note that the higher number the chunk index (int), more uncertainty the bluesky data will have.
# #' 
# #' @note
# #' In May of 2016, the directory structure for model output changed. Previously,
# #' bluesky dailiy output directories were of the following nature:
# #' 
# #' Chunk: Suppose a model has spacing = 12. In this case, Chunk = 1 means we selet the first 12 hours from each model 
# #' run -- i.e. we work with the model data from as close to initialization as possible. Chunk = 2 means we select the
# #' next 12 hours (i.e. hours 13-24) from each model run. A chunk of 3 means we select hours 25-48 from each model run. 
# #' And so on. Chunking is used to account for any innacuracies in the data that may be due to poor initial conditions.
# #' For example, older models only predicted impacts from existing sources, and did not take into account any smoke that
# #' may have already been present at the time of initialization.
# #' 
# #' \preformatted{.../standard/NAM84-0.08deg/2016050600/data/...}
# #' 
# #' During May 2016, output directories were modified and data are now found
# #' in directories similar to the following:
# #' 
# #' \preformatted{
# #' .../standard/NAM84-0.08deg/2016050600/carryover/data/...
# #' .../standard/NAM84-0.08deg/2016050600/combined/data/...
# #' .../standard/NAM84-0.08deg/2016050600/forecast/data/...
# #' }
# #' 
# #' For more recent model runs, users must specify which \code{subDir} they are interested in.
# #' 
# #' @return ws_grid object where multiple bluesky models are aggregated 
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' bs <- bluesky_aggregate('PNW-4km', 20160907, 20160910, chunk=1)
# #' gridMap(bs)
# #' }

# bluesky_aggregate <- function(model, runStart, runEnd, subDir='combined', spacing=NULL, chunk=1, param='pm25') {
  
#   # ----- Modelrun Download -------------------------------------------------------
  
#   # Different models have different default spacing
#   defaultSpacing <- list('PNW-1.33km'=24,
#                          'PNW-4km'=24,
#                          'CANSAC-2km'=12,
#                          'CANSAC-6km'=12,
#                          'NAM36-0.08deg'=12,
#                          'NAM84-0.08deg'=12,
#                          'NAM84-0.15deg'=12,
#                          'NAM-4km'=12)
  
#   if ( is.null(spacing) ) spacing <- defaultSpacing[[model]]
  
#   # Just in case users entered non-integer value for chunk, convert the chunk as integer value
#   chunk <- as.integer(chunk)
  
#   # Start times
#   timeAdder <- ifelse(nchar(as.character(runStart))==8,1,0)
#   runStart <- parseDatetime(runStart)
#   t0 <- runStart + timeAdder*lubridate::hours(1)
#   runStart <- lubridate::floor_date(runStart,unit=paste0(spacing," hours"))
#   runStart <- runStart - lubridate::hours(chunk-1)*defaultSpacing[[model]]
  
#   # End times
#   timeAdder <- ifelse(nchar(as.character(runEnd))==8,1,0)
#   runEnd <- parseDatetime(runEnd) + timeAdder*lubridate::hours(12)
#   tf <- runEnd + timeAdder*lubridate::dhours(12)
  
#   # Format switchover dates
#   switchoverDates <- list('PNW-1.33km'=2016050700,
#                           'PNW-4km'=2016050700,
#                           'CANSAC-2km'=2016050700,
#                           'CANSAC-6km'=2016050700,
#                           'NAM36-0.08deg'=2016050900,
#                           'NAM84-0.08deg'=2016050900,
#                           'NAM84-0.15deg'=2016050700,
#                           'NAM-4km'=2016050900)
  
#   # Sanity check
#   if ( !model %in% names(defaultSpacing) ) {
#     stop(paste0("bluesky_aggregate: Model '",model,"' is not recognized.")) 
#   }
    
#   # Create an empty list for model data and count the number of model runs to be downloaded
#   bsList <- list()
  
#   timeStep <- paste(defaultSpacing[[model]],'hours',sep=' ')
#   count <- length(strftime(seq(runStart, runEnd, by=timeStep), '%Y%m%d%H', tz='GMT'))

#   dims <- c(1,1,1)
  
#   # Download the model runs. Skip to the next model run if the model run doesn't exist
#   for (i in 1:count) {
    
#     modelRun <- runStart + lubridate::dhours((i-1)*defaultSpacing[[model]])
#     modelRun <- format(modelRun, '%Y%m%d%H')
    
#     if ( as.integer(modelRun) < switchoverDates[[model]] ) {
#       tempSubDir <- NULL
#     } else {
#       tempSubDir <- subDir
#     }
    
    
#     result <- try( bsList[[modelRun]] <- bluesky_load(model=model, modelRun=modelRun, subDir=tempSubDir) )
#     if( class(result)[1] == 'try-error' ) {
#       print(paste0('modelRun ', modelRun, ' does not exist...', ' Skipping to the next modelRun'))
#       next
#     }
    
#     # Sanity check -- X and Y dimensions must be the same
#     if (i == 1) {
#       dims <- dim(bsList[[modelRun]]$data[[param]])
#     } else {
#       newDims <- dim(bsList[[modelRun]]$data[[param]])
#       if ( !all(newDims[1:2] == dims[1:2]) ) {
#         dimString <- paste0(dims,collapse="-")
#         newDimString <- paste0(newDims,collapse="-")
#         stop(paste0("bluesky_aggregate: Model Run '",modelRun,"' has incompatible dimensions: ",dimString," != ",newDimString))
#       }
#     }
    
#   }
  
#   # ----- Processing the downloaded model runs -------------------------------------------------------
  
#   # First, count the number of model runs that are downloaded
#   newCount <- length(names(bsList))
    
#   # Extract the constant information such as longitude and latitude from the first object from the list
#   # Constract the empty list of pm25 and timeSets for aggregating them later
#   lon <- bsList[[1]]$longitude
#   lat <- bsList[[1]]$latitude
#   elevation <- bsList[[1]]$elevation
#   model <- bsList[[1]]$model
#   modelRun <- paste(bsList[[1]]$modelRun, bsList[[newCount]]$modelRun, sep="_")
#   deltaLon <- bsList[[1]]$deltaLon
#   deltaLat <- bsList[[1]]$deltaLat
  
#   pm25List <- list()
#   timeSets <- list()
  
#   # Sanity check -- spacing value must be less than the entire time length of the model
#   if ( spacing > length(bsList[[1]]$time) ) {   
#     stop("spacing should not be larger than the entire time length of the model")
#   }
  
#   # If chunk = 1, then take the first time indexes that corresponds to the spacing
#   # Same principle applies to the pm25 3D array.
#   if ( chunk==1 ) {
    
#     # TODO: add loop over the following to account for multiple parameters
#     bsList[[1]]$time <- bsList[[1]]$time[1:spacing]
#     bsList[[1]]$data[[param]] <- bsList[[1]]$data[[param]][,,1:spacing]
    
#     # If chunk > 1, then shift the starting index to the appropriate amount
#     # Also, subtracts all the elements that are N/A
#   } else if (chunk > 1) {
    
#     startIndex <- spacing*(chunk-1)+1
    
#     if ( startIndex > length(bsList[[1]]$time) ) {
#       stop("Chunk number too large: The starting time point is not found in the first modelRun")
#     }
    
#     endIndex <- spacing*chunk
#     bsList[[1]]$time <- bsList[[1]]$time[startIndex:endIndex]
#     bsList[[1]]$time <- bsList[[1]]$time[!is.na(bsList[[1]]$time)]
#     pm25endIndex <- startIndex + (length(bsList[[1]]$time) - 1)
#     bsList[[1]]$data[[param]] <- bsList[[1]]$data[[param]][,,startIndex:pm25endIndex]
    
#   }
  
#   # Finds the index of the 2nd object in the bsList that corresponds to the last time index of the 1st object
#   # If there exists an index, then repeat the process for the rest of the objects in bsList 
#   # and use the index information to subset the time and pm25
#   if ( !length(which(bsList[[1]]$time[length(bsList[[1]]$time)] == bsList[[2]]$time)) == 0 ) {
    
#     for (i in 2:newCount) {
#       startIndex <- which(bsList[[i-1]]$time[length(bsList[[i-1]]$time)] == bsList[[i]]$time) + 1
#       endIndex <- startIndex + (spacing - 1)
#       bsList[[i]]$time <- bsList[[i]]$time[startIndex:endIndex]
#       # Notice that in order to avoid the error from subsetting the 3D array, subtracts the NA elements from the time
#       # interval and create a new endIndex for subsetting pm25 array
#       bsList[[i]]$time <- bsList[[i]]$time[!is.na(bsList[[i]]$time)]
#       pm25endIndex <- startIndex + (length(bsList[[i]]$time) - 1)
#       bsList[[i]]$data[[param]] <- bsList[[i]]$data[[param]][,,startIndex:pm25endIndex]

#     # If the spacing is small enough that there does not exists a time index that connects each model run, 
#     # Create a discontinued time axis by taking first indexes that correspond to spacing value for each model run
  
#     }
    
#   } else {
    
#     for (i in 2:newCount) {
#       bsList[[i]]$time <- bsList[[i]]$time[1:spacing]
#       bsList[[i]]$data[[param]] <- bsList[[i]]$data[[param]][,,1:spacing]
#     }
    
#   }
  
#   # ----- Merging the processed time and 3D parameter data --------------------
  
#   for (modelRun in names(bsList)) {
#     timeSets[[modelRun]] <- bsList[[modelRun]]$time
#     pm25List[[modelRun]] <- bsList[[modelRun]]$data[[param]]
#   }
  
#   # NOTE:  dplyr converts from POSIXct to numeric.
#   # NOTE:  We force converstion to numeric in case dplyr ever changes and then convert back to POSIXct.
#   tz <- lubridate::tz(timeSets[[1]])
#   timeAxis <- as.numeric(dplyr::combine(timeSets))
#   timeAxis <- as.POSIXct(timeAxis, tz=tz, origin=lubridate::origin)
  
#   pm25 <- abind::abind(pm25List) # NOTE: individual grid cell pm25 data gets stamped with model source here
#                                  # e.g. try names(pm25[1,1,])
  
#   # ----- Subsetting the merged time and parameter data -----------------------
  
#   if ( length(which(timeAxis == t0, arr.ind = TRUE)) == 0 ) {
    
#     tIndex1 <- 1
    
#   } else {
    
#     tIndex1 <- which(timeAxis == t0, arr.ind = TRUE)
    
#   }
  
#   tIndex2 <- which(timeAxis == tf, arr.ind = TRUE)
  
#   timeAxis <- timeAxis[tIndex1:tIndex2]
#   pm25 <- pm25[,,tIndex1:tIndex2]
  
  
#   # Fill in missing periods with NAs
#   newTimeAxis <- seq(timeAxis[1],timeAxis[length(timeAxis)],by="1 hour")
#   missingMask <- !(newTimeAxis %in% timeAxis)
#   newPM25 <- array(data = NA,dim = c(dim(pm25)[1:2],length(newTimeAxis)))
  
#   for (ts in 1:length(newTimeAxis)) {
#     if ( missingMask[ts]==0 ) { # if not missing
#       time <- newTimeAxis[ts]
#       newPM25[,,ts] <- pm25[,,which(timeAxis==time)]
#     }
#   }
  
#   timeAxis <- newTimeAxis
#   pm25 <- newPM25
  
#   # ----- Create and return the ws_grid object -------------------------------------------------------
  
#   return(structure(list(longitude=lon,
#                         latitude=lat,
#                         elevation=elevation,
#                         time=timeAxis,
#                         data=list(pm25=pm25),
#                         model=model,
#                         modelRun=modelRun,
#                         deltaLon=deltaLon,
#                         deltaLat=deltaLat),
#                         # modelRunIndex=data.frame(hour=timeAxis, model_and_index=names(pm25[1,1,]))),
#                    class=c("ws_grid", "list")))
# }
