#' @export
#' @importFrom MazamaCoreUtils logger.warn logger.error
#' 
#' @title Aggregate multiple BlueSky model runs into a single bs_grid object
#' 
#' @param dailyOutputDir BlueSky web directory.
#' @param model Model identifier.
#' @param firstModelRun Datestamp of first model run. Supports either 10 digits
#' format (e.g: 2015072100) or 8 digit format (e.g: 20150721).
#' @param lastModelRun Datestamp of the last model run.
#' @param subDir Subdirectory path containing netcdf data.
#' @param param Parameter name.
#' @param download Logical specifying whether to download and convert data if it
#' is not found locally.
#' @param cleanup Logical specifying whether to remove the original files.
#' @param baseUrl Base URL for BlueSky output.
#' @param quiet If \code{TRUE}, suppress status messages (if any), and the
#' progress bar.
#' @param spacing Time spacing (hrs) between each model run -- overrides default 
#' values
#' @param chunk The portion of the time (of width = 'spacing') to be chosen from 
#' each model run. If \code{spacing = 12}, then \code{chunk = 1} corresponds to 
#' the first 12 hours of the each model run, \code{chunk = 2} corresponds to the 
#' next 12 hours of each model run, and so on. See the note below for further 
#' explanation.
#' 
#' @description Combines multiple bluesky model runs tp produce a single 
#' \emph{bs_grid} object. Users can set the start date, end date, the hourly 
#' spacing between model runs, and the chunk index for the cssection of each
#' models time axis to be used.
#' 
#' @details
#' Setting \code{cunk = 1} results in grabbing the initial portion of each model
#' run. Setting chunk to higher numbers utilizes times further out in the the
#' forecast of each model run implying more uncertainty.
#' 
#' @section Chunk: 
#' Suppose a model is run every 12 hours. For this model \code{spacing = 12}. 
#' In this case, \code{chunk = 1} means we selet the first 12 hours from each 
#' model run -- i.e. we work with the model data from as close to initialization 
#' as possible. Setting \code{chunk = 2} means we select the next 12 hours 
#' (i.e. hours 13-24) from each model run. Setting \code{chunk = 3} means we 
#' select hours 25-48 from each model run and so on.
#' 
#' Chunking is can be used to account for any innacuracies in the data that may 
#' be due to poor initial conditions. For example, older models only predicted 
#' impacts from existing sources, and did not take into account any smoke that
#' may have already been present at the time of initialization. In this case,
#' setting \code{chunk = 2} produced better results.
#' 
#' @return \emph{bs_grid} object aggregating multiple bluesky model runs.
#' 
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bs <- bluesky_aggregate(model = "PNW-4km", 20190907, 20190910, chunk = 1)
#' gridMap(bs)
#' }

bluesky_aggregate <- function(
  dailyOutputDir = "standard",
  model, 
  firstModelRun = NULL, 
  lastModelRun = NULL, 
  subDir = "combined", 
  param = "pm25",
  download = TRUE,
  cleanup = TRUE,
  baseUrl = "https://haze.airfire.org/bluesky-daily/output",
  quiet = FALSE,
  spacing = NULL, 
  chunk = 1
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(dailyOutputDir)
  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(subDir)
  MazamaCoreUtils::stopIfNull(baseUrl)
  MazamaCoreUtils::stopIfNull(param)
  
  # Default to download
  if ( !is.logical(download) ) 
    download <- TRUE
  
  # Default to cleanup
  if ( !is.logical(cleanup) ) 
    cleanup <- TRUE
  
  # Default to verbose
  if ( !is.logical(quiet) ) 
    quiet <- FALSE
  
  # Model-specific spacing
  if ( is.null(spacing) && dailyOutputDir == "standard" ) {
    spacing <= switch(model,
                      "AK-12km" = 6,
                      "CANSAC-1.33km" = 12,
                      "CANSAC-4km" = 12,
                      "DRI1.33km-CMAQ" = 24,
                      "DRI4km-CMAQ" = 24,
                      "GFS-0.15deg-CanadaUSA-p25deg-68N" = 12,
                      "GFS-0.15deg" = 12,
                      "NAM-3km" = 12,
                      "NAM84-0.15deg" = 12,
                      "PNW-1.33km" = 12,
                      "PNW-4km" = 12,
                      "PNW1.33km-CMAQ" = 24,
                      "PNW4km-CMAQ" = 24
    )
  }
  
  # Did we identify it?
  MazamaCoreUtils::stopIfNull(spacing)
  
  # Just in case
  chunk <- as.integer(chunk)
  
  # ----- Create time axis -----------------------------------------------------
  
  # All timestamps are UTC
  by <- paste0(spacing, " hours")
  starttime <- MazamaCoreUtils::parseDatetime(firstModelRun, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(lastModelRun, timezone = "UTC")
  runTimes <- seq(starttime, endtime, by = paste0(spacing, " hours"))
  modelRuns <- strftime(runTimes, "%Y%m%d%H", tz = "UTC")
  
  # ----- Download model runs --------------------------------------------------
  
  # Create an empty list for model data
  bsList <- list()
  
  dims <- c(1,1,1)
  
  iteration <- 0
  for (modelRun in modelRuns) {
    
    iteration <- iteration + 1
    
    result <- try({
      bsList[[modelRun]] <- bluesky_load(
        dailyOutputDir = dailyOutputDir,
        model = model,
        modelRun = modelRun, 
        subDir = subDir,
        param = param,
        download = download,
        cleanup = cleanup,
        baseUrl = baseUrl,
        quiet = quiet
      )
    },  silent = TRUE )
    if( "try-error" %in% class(result) ) {
      if ( MazamaCoreUtils::logger.isInitialized() )
        logger.warn("Could not download %s/%s ... skipping", model, modelRun)
      next
    }
    
    # Sanity check -- X and Y dimensions must be the same
    if ( iteration == 1 ) {
      dims <- dim(bsList[[modelRun]]$data[[param]])
    } else {
      newDims <- dim(bsList[[modelRun]]$data[[param]])
      if ( !all(newDims[1:2] == dims[1:2]) ) {
        dimString <- paste0(dims, collapse = "-")
        newDimString <- paste0(newDims, collapse = "-")
        err_msg <- paste0(
          "bluesky_aggregate: Model Run '", modelRun, 
          "' has incompatible dimensions: ",
          dimString, " != ", newDimString)
        if ( MazamaCoreUtils::logger.isInitialized() )
          logger.error(err_msg)
        stop(err_msg)
      }
    }
    
  }

  
  
  
  
 stop("WORK IN PROGRESS") 
  
  
  
  
  
    
  # # ----- Process model runs ---------------------------------------------------
  # 
  # # How many model runs?
  # newCount <- length(names(bsList))
  # 
  # # Extract the constant information such as longitude and latitude from the first object from the list
  # # Constract the empty list of pm25 and timeSets for aggregating them later
  # lon <- bsList[[1]]$longitude
  # lat <- bsList[[1]]$latitude
  # elevation <- bsList[[1]]$elevation
  # model <- bsList[[1]]$model
  # modelRun <- paste(bsList[[1]]$modelRun, bsList[[newCount]]$modelRun, sep="_")
  # deltaLon <- bsList[[1]]$deltaLon
  # deltaLat <- bsList[[1]]$deltaLat
  # 
  # pm25List <- list()
  # timeSets <- list()
  # 
  # # Sanity check -- spacing value must be less than the entire time length of the model
  # if ( spacing > length(bsList[[1]]$time) ) {   
  #   stop("spacing should not be larger than the entire time length of the model")
  # }
  # 
  # # If chunk = 1, then take the first time indexes that corresponds to the spacing
  # # Same principle applies to the pm25 3D array.
  # if ( chunk==1 ) {
  #   
  #   # TODO: add loop over the following to account for multiple parameters
  #   bsList[[1]]$time <- bsList[[1]]$time[1:spacing]
  #   bsList[[1]]$data[[param]] <- bsList[[1]]$data[[param]][,,1:spacing]
  #   
  #   # If chunk > 1, then shift the starting index to the appropriate amount
  #   # Also, subtracts all the elements that are N/A
  # } else if (chunk > 1) {
  #   
  #   startIndex <- spacing*(chunk-1)+1
  #   
  #   if ( startIndex > length(bsList[[1]]$time) ) {
  #     stop("Chunk number too large: The starting time point is not found in the first modelRun")
  #   }
  #   
  #   endIndex <- spacing*chunk
  #   bsList[[1]]$time <- bsList[[1]]$time[startIndex:endIndex]
  #   bsList[[1]]$time <- bsList[[1]]$time[!is.na(bsList[[1]]$time)]
  #   pm25endIndex <- startIndex + (length(bsList[[1]]$time) - 1)
  #   bsList[[1]]$data[[param]] <- bsList[[1]]$data[[param]][,,startIndex:pm25endIndex]
  #   
  # }
  # 
  # # Finds the index of the 2nd object in the bsList that corresponds to the last time index of the 1st object
  # # If there exists an index, then repeat the process for the rest of the objects in bsList 
  # # and use the index information to subset the time and pm25
  # if ( !length(which(bsList[[1]]$time[length(bsList[[1]]$time)] == bsList[[2]]$time)) == 0 ) {
  #   
  #   for (i in 2:newCount) {
  #     startIndex <- which(bsList[[i-1]]$time[length(bsList[[i-1]]$time)] == bsList[[i]]$time) + 1
  #     endIndex <- startIndex + (spacing - 1)
  #     bsList[[i]]$time <- bsList[[i]]$time[startIndex:endIndex]
  #     # Notice that in order to avoid the error from subsetting the 3D array, subtracts the NA elements from the time
  #     # interval and create a new endIndex for subsetting pm25 array
  #     bsList[[i]]$time <- bsList[[i]]$time[!is.na(bsList[[i]]$time)]
  #     pm25endIndex <- startIndex + (length(bsList[[i]]$time) - 1)
  #     bsList[[i]]$data[[param]] <- bsList[[i]]$data[[param]][,,startIndex:pm25endIndex]
  #     
  #     # If the spacing is small enough that there does not exists a time index that connects each model run, 
  #     # Create a discontinued time axis by taking first indexes that correspond to spacing value for each model run
  #     
  #   }
  #   
  # } else {
  #   
  #   for (i in 2:newCount) {
  #     bsList[[i]]$time <- bsList[[i]]$time[1:spacing]
  #     bsList[[i]]$data[[param]] <- bsList[[i]]$data[[param]][,,1:spacing]
  #   }
  #   
  # }
  # 
  # # ----- Merging the processed time and 3D parameter data --------------------
  # 
  # for (modelRun in names(bsList)) {
  #   timeSets[[modelRun]] <- bsList[[modelRun]]$time
  #   pm25List[[modelRun]] <- bsList[[modelRun]]$data[[param]]
  # }
  # 
  # # NOTE:  dplyr converts from POSIXct to numeric.
  # # NOTE:  We force converstion to numeric in case dplyr ever changes and then convert back to POSIXct.
  # tz <- lubridate::tz(timeSets[[1]])
  # timeAxis <- as.numeric(dplyr::combine(timeSets))
  # timeAxis <- as.POSIXct(timeAxis, tz=tz, origin=lubridate::origin)
  # 
  # pm25 <- abind::abind(pm25List) # NOTE: individual grid cell pm25 data gets stamped with model source here
  # # e.g. try names(pm25[1,1,])
  # 
  # # ----- Subsetting the merged time and parameter data -----------------------
  # 
  # if ( length(which(timeAxis == t0, arr.ind = TRUE)) == 0 ) {
  #   
  #   tIndex1 <- 1
  #   
  # } else {
  #   
  #   tIndex1 <- which(timeAxis == t0, arr.ind = TRUE)
  #   
  # }
  # 
  # tIndex2 <- which(timeAxis == tf, arr.ind = TRUE)
  # 
  # timeAxis <- timeAxis[tIndex1:tIndex2]
  # pm25 <- pm25[,,tIndex1:tIndex2]
  # 
  # 
  # # Fill in missing periods with NAs
  # newTimeAxis <- seq(timeAxis[1],timeAxis[length(timeAxis)],by="1 hour")
  # missingMask <- !(newTimeAxis %in% timeAxis)
  # newPM25 <- array(data = NA,dim = c(dim(pm25)[1:2],length(newTimeAxis)))
  # 
  # for (ts in 1:length(newTimeAxis)) {
  #   if ( missingMask[ts]==0 ) { # if not missing
  #     time <- newTimeAxis[ts]
  #     newPM25[,,ts] <- pm25[,,which(timeAxis==time)]
  #   }
  # }
  # 
  # timeAxis <- newTimeAxis
  # pm25 <- newPM25
  # 
  # # ----- Create and return the bs_grid object -------------------------------------------------------
  # 
  # return(structure(list(longitude=lon,
  #                       latitude=lat,
  #                       elevation=elevation,
  #                       time=timeAxis,
  #                       data=list(pm25=pm25),
  #                       model=model,
  #                       modelRun=modelRun,
  #                       deltaLon=deltaLon,
  #                       deltaLat=deltaLat),
  #                  # modelRunIndex=data.frame(hour=timeAxis, model_and_index=names(pm25[1,1,]))),
  #                  class=c("bs_grid", "list")))
  
}
