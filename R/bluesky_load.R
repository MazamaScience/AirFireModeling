bluesky_load <- function(
  filePath = NULL,
  modelRun= NULL,
  parameter = NULL,
  cleanup = TRUE,
  baseUrl = "https://haze.airfire.org/bluesky-daily/output",
  dailyOutputDir = "standard",
  model = 'CANSAC-1.33km',
  subDir = "forecast",
  verbose = FALSE #SHOULD BE T
) {

  # Checks
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(dailyOutputDir)
  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(subDir)
  MazamaCoreUtils::stopIfNull(baseUrl)
  MazamaCoreUtils::stopIfNull(parameter)

  if ( is.null(modelRun) ) {
    now <- lubridate::now(tzone = "UTC")
    modelRun <- paste0(strftime(now, "%Y%m%d", tz = "UTC"), "00")
  } else {
    # Make sure we end up with YYYYmmddHH
    modelRun <- as.character(modelRun)
    length <- stringr::str_length(modelRun)[1]
    if ( length == 8 ) {
      modelRun <- paste0(modelRun,'00')
    }
  }

  # Verify YYYYmmddHH
  if ( !stringr::str_detect(modelRun, "[0-9]") ) {
    stop("'modelRun' parameter must have 10 digits")
  }

  parameter <- tolower(parameter)
  if ( parameter != "pm25" ) {
    stop(paste0(
      "Parameter '", parameter, "' is not handled yet."
    ))
  }

  # Default to cleanup
  if ( !is.logical(cleanup) )
    cleanup <- TRUE

  # Default to verbose
  if ( !is.logical(verbose) )
    verbose <- FALSE

  # ----- Handle user files -----
  if ( !is.null(filePath) ) {
    if( file.exists(filePath) ) {
      # Assimilate
      nc_path <- bluesky_assimilate( filePath = filePath,
                                     cleanup = FALSE )
    } else {
      stop(paste0(
        "Could not find ", file, ".  ",
        "Did you specify an absolute path?"
      ))
    }
  # ----- Handle download -----
  } else {
    # Bluesky download
    raw_nc_path <- bluesky_download( dailyOutputDir = dailyOutputDir,
                                     model = model,
                                     modelRun = modelRun,
                                     subDir = subDir,
                                     baseUrl = baseUrl,
                                     verbose = verbose )

    # Assimilate
    nc_path <- bluesky_assimilate( raw_nc_path,
                                   cleanup = cleanup )
  }

  # Rasterize
  model_brick <- raster::brick(nc_path)

  return(model_brick)

}
