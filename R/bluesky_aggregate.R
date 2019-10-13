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
#' spacing between model runs, and the chunk index for the section of each
#' model's time axis to be used.
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
#' # Next three lines required for bs_grid to ws_monitor conversion
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' # Now we can work as we normally would
#' setModelDataDir("~/Data/Bluesky")
#' bs <- bluesky_load(
#'   model = "PNW-4km",
#'   modelRun = 20191009,
#'   subDir = "combined",
#'   chunk = 1
#' )
#' gridMap(bs)
#' }

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  setModelDataDir("~/Data/Bluesky")
  
  dailyOutputDir <- "standard"
  model <- "PNW-4km"
  firstModelRun <- 20191009
  lastModelRun <- 20191011
  subDir <- "combined"
  parameter <- "pm25"
  download <- TRUE
  cleanup <- TRUE
  baseUrl <- "https://haze.airfire.org/bluesky-daily/output"
  quiet <- FALSE
  spacing <- NULL 
  chunk <- 1
  
}

bluesky_aggregate <- function(
  dailyOutputDir = "standard",
  model, 
  firstModelRun = NULL, 
  lastModelRun = NULL, 
  subDir = "combined", 
  parameter = "pm25",
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
  MazamaCoreUtils::stopIfNull(parameter)
  
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
    spacing <- switch(model,
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
                      "PNW-4km" = 24,
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
  starttime <- MazamaCoreUtils::parseDatetime(firstModelRun, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(lastModelRun, timezone = "UTC")
  runTimes <- seq(starttime, endtime, by = paste0(spacing, " hours"))
  modelRuns <- strftime(runTimes, "%Y%m%d%H", tz = "UTC")
  
  # ----- Load model data ------------------------------------------------------
  
  # Create an empty list for model data
  bsList <- list()
  
  dims <- c(1,1,1)
  
  iteration <- 0
  for ( modelRun in modelRuns ) {
    
    iteration <- iteration + 1
    
    # TODO:  Memory saving option would subset data on the fly rather than
    # TODO:  load in everything first.
    
    result <- try({
      bsList[[modelRun]] <- bluesky_load(
        dailyOutputDir = dailyOutputDir,
        model = model,
        modelRun = modelRun, 
        subDir = subDir,
        parameter = parameter,
        download = download,
        cleanup = cleanup,
        baseUrl = baseUrl,
        quiet = quiet
      )
    },  silent = TRUE )
    
    # NOTE:  Handle missing model runs by creating an empty bs_grid object
    # NOTE:  based on the characteristics of the first modelRun loaded.
    
    if ( "try-error" %in% class(result) ) {
      
      if ( MazamaCoreUtils::logger.isInitialized() )
        logger.warn("Could not download %s/%s ... skipping", model, modelRun)
      
      # First run must exist
      if ( iteration == 1 )
        stop(paste0(
          "Could not load first model run: ", model, "/", modelRun
        ))
      
      # Create an empty bs_grid object
      empty_bs_grid <- list(
        longitude = bsList[[1]]$longitude,
        latatitude = bsList[[1]]$latitude,
        elevation = bsList[[1]]$elevation,
        time = as.POSIXct(NA),
        data = list(),
        model = bsList[[1]]$model,
        modelRun = modelRun,
        deltaLon = bsList[[1]]$deltaLon,
        deltaLat = bsList[[1]]$deltaLat
      )
      dim <- dim(bsList[[1]]$data[[parameter]])
      dim[3] <- 1
      empty_bs_grid$data[[parameter]] <- array(data = as.numeric(NA), dim = dim)

      bsList[[modelRun]] <- empty_bs_grid
      
      next
      
    } else {
      
      # Save memory
      rm(result)
      
    }
    
    # Sanity check -- X and Y dimensions must be the same
    if ( iteration == 1 ) {
      dims <- dim(bsList[[modelRun]]$data[[parameter]])
    } else {
      newDims <- dim(bsList[[modelRun]]$data[[parameter]])
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
  
  # Sanity check -- spacing
  if ( spacing > length(bsList[[1]]$time) ) {
    stop(paste0(
      "'spacing' ",
      "must be smaller than the length of the model time axis"
    ))
  }
  
  # Sanity check -- chunk
  if ( ((chunk - 1) * spacing + 1) > length(bsList[[1]]$time) ) {
    stop(paste0(
      "'(chunk - 1) * spacing' ",
      "must be smaller than the length of the model time axis"
    ))
  }
  
  # ----- Extract chunk data ---------------------------------------------------
  
  # TODO: Support a vector of parameters
  
  chunkDataList <- list()
  
  for ( modelRun in names(bsList) ) {
    
    bs_grid <- bsList[[modelRun]]

    # Handle incomplete model runs with shorter time axes
    startIndex <- (chunk - 1) * spacing + 1
    endIndex <- min(chunk * spacing, length(bs_grid$time))
    indices <- startIndex:endIndex
    
    parameter_data <- bs_grid$data[[parameter]][,,indices]
    
    # Add NAs if this model run is incomplete
    missingCount <- spacing - length(indices)
    if ( missingCount > 0 ) {
      dims <- dim(parameter_data)
      dims[3] <- missingCount
      NA_brick <- array(data = as.numeric(NA), dim = dims)
      parameter_data <- abind::abind(parameter_data, NA_brick, along = 3)
      rm(NA_brick) # save memmory
    }
    
    chunkDataList[[modelRun]] <- parameter_data
    
  }
  
  # Save memory
  rm(bs_grid)
  rm(parameter_data)
    
  # ----- Assemble aggregated bs_grid ------------------------------------------
  
  modelCount <- length(names(bsList))
  
  # Extract the constant information from the first bs_grid object int the list.
  bs_grid <- list(
    longitude = bsList[[1]]$longitude,
    latatitude = bsList[[1]]$latitude,
    elevation = bsList[[1]]$elevation,
    time = as.POSIXct(NA),
    data = list(),
    model = bsList[[1]]$model,
    modelRun = paste0(bsList[[1]]$modelRun, "_", bsList[[modelCount]]$modelRun),
    deltaLon = bsList[[1]]$deltaLon,
    deltaLat = bsList[[1]]$deltaLat
  )
  
  # Add a complete time axis
  starttime <- MazamaCoreUtils::parseDatetime(firstModelRun, timezone = "UTC")
  endtime <- 
    MazamaCoreUtils::parseDatetime(lastModelRun, timezone = "UTC") +
    lubridate::dhours(chunk * spacing - 1)
  bs_grid$time <- seq(starttime, endtime, by = "1 hour")
  
  # Add the combined data
  bs_grid$data <- abind::abind(chunkDataList)
  
  # ----- Return ---------------------------------------------------------------
  
  bs_grid <- structure(bs_grid, class = c("bs_grid", "list") )
  
  return(bs_grid)
  
}
