#' @export
#' 
#' @title Load BlueSky NetCDF file and return contents as a bs_grid.
#' 
#' @param dailyOutputDir BlueSky web directory.
#' @param model Model identifier.
#' @param modelRun Date code as "YYYYMMDDHH" (integer or character).
#' @param subDir Subdirectory path containing netcdf data.
#' @param param Parameter name.
#' @param download Logical specifying whether to download and convert data if it
#' is not found locally.
#' @param cleanup Logical specifying whether to remove the original files.
#' @param filePath Absolute path of the local bluesky .nc file 
#' \emph{(not "_V2.nc")} for special occasions.
#' @param baseUrl Base URL for BlueSky output.
#' @param quiet If \code{TRUE}, suppress status messages (if any), and the
#' progress bar.
#' 
#' @description Reads in a "_V2" BlueSky output NetCDF file and ingests the 
#' data, returning it in a list of class \code{bs_grid}.
#' 
#' @details Data are searched for in the package internal \code{dataDir} which 
#' can be set with \code{setModelDataDir()}.
#' 
#' #' On 2019-10-11, vailable model identifiers include the following:
#' \itemize{
#'   \item{AK-12km}
#'   \item{CANSAC-1.33km}
#'   \item{CANSAC-4km}
#'   \item{DRI1.33km-CMAQ}
#'   \item{DRI4km-CMAQ}
#'   \item{GFS-0.15deg-CanadaUSA-p25deg-68N}
#'   \item{GFS-0.15deg}
#'   \item{NAM-3km}
#'   \item{NAM84-0.15deg}
#'   \item{PNW-1.33km}
#'   \item{PNW-4km}
#'   \item{PNW1.33km-CMAQ}
#'   \item{PNW4km-CMAQ}
#' }
#' 
#' @return List of class \code{bs_grid} with the following elements:
#' \itemize{
#'   \item{lon -- 1D vector of longitudes (X)}
#'   \item{lat -- 1D vector of latitudes (Y)}
#'   \item{time -- 1D vector of POSIXct times (T)}
#'   \item{pm25 -- 3D array of PM2.5 values (X-Y-T)}
#'   \item{model -- character name of model used to generate the data}
#'   \item{modelRun -- character date + hour when the model was run}
#'   \item{deltaLon -- longitude axis grid cell spacing}
#'   \item{deltaLat -- latitude axis grid cell spacing}
#' }
#' 
#' BlueSky output files are found in directories with the following
#' structure:
#' 
#' \preformatted{
#' .../standard/NAM84-0.08deg/2016050600/carryover/data/...
#' .../standard/NAM84-0.08deg/2016050600/combined/data/...
#' .../standard/NAM84-0.08deg/2016050600/forecast/data/...
#' }
#' 
#' @seealso \link{bluesky_download}
#' @seealso \link{bluesky_toV2}
#' @seealso \link{setModelDataDir}
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bs_grid <- bluesky_load(model = "PNW-1.33km", modelRun = 2019090900)
#' }

bluesky_load <- function(
  dailyOutputDir = "standard",
  model = "PNW-1.33km",
  modelRun = NULL,
  subDir = "combined",
  param = "pm25",
  download = TRUE,
  cleanup = TRUE,
  filePath = NULL,
  baseUrl = "https://haze.airfire.org/bluesky-daily/output",
  quiet = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(dailyOutputDir)
  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(subDir)
  MazamaCoreUtils::stopIfNull(baseUrl)
  MazamaCoreUtils::stopIfNull(param)
  
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
  
  # Default to download
  if ( !is.logical(download) ) 
    download <- TRUE
  
  # Default to cleanup
  if ( !is.logical(cleanup) ) 
    cleanup <- TRUE
  
  # Default to verbose
  if ( !is.logical(quiet) ) 
    quiet <- FALSE
  
  # ----- Handle user files ----------------------------------------------------
  
  if ( !is.null(filePath) ) {
    
    if ( file.exists(file) ) {
      newPath <- bluesky_toV2(filePath = filePath, cleanup = FALSE)
      filePath <- newPath
    } else {
      stop(paste0(
        "Could not find ", file, ".  ",
        "Did you specify an absolute path?"
      ))
    }
    
  } else {
    
    fileName <- paste0(model, "_", modelRun, "_V2.nc")
    filePath <- file.path(getModelDataDir(), fileName)
    
  }
  
  # ----- Open or download the file --------------------------------------------
  
  if ( file.exists(filePath) ) {
    
    nc <- ncdf4::nc_open(filePath)
    
  } else {
    
    if ( download ) {
      
      # Extract information from filename
      fileName <- basename(filePath)
      matchMatrix <- stringr::str_match(fileName,'(.+)_([0-9]+)(_.+).nc')
      model <- matchMatrix[,2]
      modelRun <- matchMatrix[,3]
      suffix <- matchMatrix[,4] # Not used
      
      # Download and convert data
      oldFilePath <- bluesky_download(dailyOutputDir ,model, modelRun, subDir)
      newFilePath <- bluesky_toV2(oldFilePath, cleanup)
      
      # Sanity check
      if ( newFilePath != filePath ) {
        stop(paste0(
          "bluesky_load: filepath mismatch: filePath = '", filePath,
          "' but newFilePath = '", newFilePath, "'."
        ))
      }
      
      # Open file
      nc <- ncdf4::nc_open(newFilePath)      
      
    } else {
      
      stop(paste0(
        "bluesky_load: file '", fileName,
        "' not found and download = FALSE."
      ))
      
    }
    
  }
  
  # TODO:  Handle errors
  
  # ----- Read in data ---------------------------------------------------------
  
  data <- list()
  
  # NOTE:  All netcdf variables are returned as class "array" and must be 
  # NOTE:  convnerted to numeric or POSIXct as needed.
  
  lon <- as.numeric(ncdf4::ncvar_get(nc, "lon"))
  lat <- as.numeric(ncdf4::ncvar_get(nc, "lat"))
  elevation <- as.numeric(ncdf4::ncvar_get(nc, "elevation"))
  time <- ncdf4::ncvar_get(nc, "time")
  time <- as.POSIXct(time, origin = "1970-01-01", tz = "UTC")
  
  # NOTE:  Sometimes the time variable has no data. In this case we create an
  # NOTE:  hourly axis starting at modelRun.
  
  if ( any(is.na(time)) ) {
    from <- MazamaCoreUtils::parseDatetime(modelRun, timezone = "UTC")
    time <- seq(from, by = "hours", length.out = length(time))
  }
  
  # TODO:  Handle parameters other than "PM25"
  
  pm25 <- ncdf4::ncvar_get(nc, "PM25")
  if ( param == 'pm25' ) {
    data$pm25 <- pm25
  } else {
    stop(paste0("bluesky_load: Parameter '", param, "' is not handled yet."))
  }
  
  # Close the file now that everything is in memory
  ncdf4::nc_close(nc)
  
  deltaLon <- lon[2] - lon[1]
  deltaLat <- lat[2] - lat[1]
  
  # ----- Return ---------------------------------------------------------------
  
  bs_grid <- list(
    longitude = lon,
    latitude = lat,
    elevation = elevation,
    time = time,
    data = data,
    model = model,
    modelRun = modelRun,
    deltaLon = deltaLon,
    deltaLat = deltaLat
  )
  
  return(structure(bs_grid, class = c("bs_grid", "list")))
  
}

