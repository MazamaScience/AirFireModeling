#' @export
#' @importFrom stats na.omit
#' 
#' @title Load BlueSky NetCDF file and return contents as a bs_grid.
#' 
#' @param dailyOutputDir BlueSky web directory.
#' @param model Model identifier.
#' @param modelRun Date code as "YYYYMMDDHH" (integer or character).
#' @param subDir Subdirectory path containing netcdf data.
#' @param parameter Parameter name.
#' @param xlim Optional longitude range.
#' @param ylim Optional latitude range.
#' @param timesteps Optional vector of timesteps to be loaded. Defaults to all
#' available timesteps in the file. 
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
#' Data are searched for in the package internal \code{dataDir} which can be set 
#' with \code{setModelDataDir()}. If no data are found in the \code{dataDir}, 
#' and \code{download = TRUE} an attempt is made to download data.
#' 
#' If \code{timesteps} are specified, the returned \emph{bs_grid} object will
#' contain data associated with those timesteps. Data missing in the original
#' model run will be replaced with NA values.
#' 
#' @details
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
#'   \item{data -- list of 3D arrays of data values (X-Y-T)}
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
#' 
#' xlim <- c(-118, -114)
#' ylim <- c(45, 48)
#' bs_grid <- bluesky_load(
#'   model = "PNW-1.33km", 
#'   modelRun = 2019100900,
#'   xlim = xlim, 
#'   ylim = ylim
#' )
#' grid_map(bs_grid, tMask = 1:24)
#' }

bluesky_load <- function(
  dailyOutputDir = "standard",
  model = "PNW-1.33km",
  modelRun = NULL,
  subDir = "combined",
  parameter = "pm25",
  xlim = NULL,
  ylim = NULL,
  timesteps = NULL,
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
  
  # ----- BlueSky model structure ----------------------------------------------
  
  # On 2019-10-13 
  # > print(nc)
  # File /Users/jonathan/Data/Bluesky/PNW-4km_2019100900_V2.nc (NC_FORMAT_CLASSIC):
  #   
  #   1 variables (excluding dimension variables):
  #     float PM25[lon,lat,elevation,time]   
  #       units: ug/m^3
  #       _FillValue: -1.00000001504747e+30
  # 
  #   4 dimensions:
  #     lon  Size:514
  #       units: Degrees East
  #       long_name: lon
  #     lat  Size:264
  #       units: Degrees North
  #       long_name: lat
  #     elevation  Size:1
  #       units: Meters from sea level
  #       long_name: elevation
  #     time  Size:71
  #       units: seconds from 1970-1-1
  #       long_name: time
  
  # ----- Read in axis data ----------------------------------------------------
  
  # NOTE:  All netcdf variables are returned as class "array" and must be 
  # NOTE:  convnerted to numeric or POSIXct as needed.
  
  lon <- as.numeric(ncdf4::ncvar_get(nc, "lon"))
  lat <- as.numeric(ncdf4::ncvar_get(nc, "lat"))
  elevation <- as.numeric(ncdf4::ncvar_get(nc, "elevation"))
  time <- ncdf4::ncvar_get(nc, "time")
  time <- as.POSIXct(time, origin = "1970-01-01", tz = "UTC")
  
  # NOTE:  Sometimes the time variable has missing values. In this case we 
  # NOTE:  create an hourly axis starting at modelRun.
  
  # TODO:  Or does this imply that the model run skipped a step???
  if ( any(is.na(time)) ) {
    warning(paste0(
      "Missing time axis values found in: ",
      model, "/", modelRun
    ))
    from <- MazamaCoreUtils::parseDatetime(modelRun, timezone = "UTC")
    time <- seq(from, by = "hours", length.out = length(time))
  }
  
  # In case we need them later
  lon_originalModelAxis <- lon
  lat_originalModelAxis <- lat
  time_originalModelAxis <- time
  
  # ----- Handle xlim, ylim ----------------------------------------------------
  
  if ( is.null(xlim) ) {
    
    start_x <- 1
    count_x <- length(lon)
    
  } else {
    
    # Any values lower than xlim[1]? Find the index of the biggest one. 
    # Otherwise use the start.
    if ( any(lon < xlim[1]) ) {
      index_lonLo <- max(which(lon < xlim[1]))
    } else {
      index_lonLo <- 1
    }
    
    if ( any(lon > xlim[2]) ) {
      index_lonHi <- min(which(lon > xlim[2]))
    } else {
      index_lonHi <- length(lon)
    }
    
    start_x <- index_lonLo
    count_x <- index_lonHi - index_lonLo + 1
    
    # Subset lon varaible
    lon <- lon[index_lonLo:index_lonHi]

  }
  
  if ( is.null(ylim) ) {
    
    start_y <- 1
    count_y <- length(lat)
    
  } else {
    
    if ( any(lat < ylim[1]) ) {
      index_latLo <- max(which(lat < ylim[1]))
    } else {
      index_latLo <- 1
    }
    
    if ( any(lat > ylim[2]) ) {
      index_latHi <- min(which(lat > ylim[2]))
    } else {
      index_latHi <- length(lat)
    }
    
    start_y <- index_latLo
    count_y <- index_latHi - index_latLo + 1
    
    # Subset lat varaible
    lat <- lat[index_latLo:index_latHi]

  }
  
  # ----- Read in parameter data -----------------------------------------------
  
  if ( is.null(timesteps) ) {
    
    timeAxis <- time
    
    parameter_data <- ncdf4::ncvar_get(
      nc, 
      varid = "PM25",
      start = c(start_x, start_y, 1, 1),
      count = c(count_x, count_y, -1, -1),
      verbose = FALSE,
      signedbyte = TRUE,
      collapse_degen = TRUE,
      raw_datavals = FALSE
    )
    
  } else {
    
    timeAxis <- timesteps
    
    # Find indices of timesteps
    indices_t <- match(timesteps, time)

    # Sanity check -- we have the right modelRun
    if ( all(is.na(indices_t)) )
      stop(paste0(
        "No requested 'timesteps' found in the modelRun time axis."
      ))
    
    # Avoid problems with 'collapse_degen' below
    if ( length(indices_t) < 2 )
      stop(paste0(
        "Only 1 'timesteps' found in the modelRun time axis."
      ))
    
    # Sanity check -- no skipped timesteps
    if ( !all(diff(na.omit(indices_t)) == 1) )
      stop(paste0(
        "Requested 'timesteps' are not sequential."
      ))

    # Read in available netcdf data
    startIndex_t <- min(indices_t, na.rm = TRUE)
    validCount_t <- sum(!is.na(indices_t))

    parameter_data <- ncdf4::ncvar_get(
      nc, 
      varid = "PM25",
      start = c(start_x, start_y, 1, startIndex_t),
      count = c(count_x, count_y, -1, validCount_t),
      verbose = FALSE,
      signedbyte = TRUE,
      collapse_degen = TRUE,
      raw_datavals = FALSE
    )
    
    # NOTE:  parameter_data is now 3D because of 'collapse_degen == TRUE'
    
    # Prepend with NA
    if ( is.na(indices_t[1]) ) {
      index_firstValid <- min(which(!is.na(indices_t)))
      dims <- dim(parameter_data)
      dims[3] <- index_firstValid - 1
      NA_brick <- array(data = as.numeric(NA), dim = dims)
      parameter_data <- abind::abind(NA_brick, parameter_data, along = 3) # no Z axis
      rm(NA_brick) # save memmory
    }
    
    # Postpend with NA
    if ( is.na(indices_t[length(indices_t)]) ) {
      index_lastValid <- max(which(!is.na(indices_t)))
      dims <- dim(parameter_data)
      dims[3] <- length(indices_t) - index_lastValid
      NA_brick <- array(data = as.numeric(NA), dim = dims)
      parameter_data <- abind::abind(parameter_data, NA_brick, along = 3) # no Z axis
      rm(NA_brick) # save memmory
    }
    
  }
  
  # TODO:  Figure out why this was causing memory problems that resulted in
  # TODO:  (memory?) errors resulting in complaints from MazamaCoreUtils about
  # TODO:  not understanding "UTC".
  
  # Close the file now that everything is in memory
  ncdf4::nc_close(nc)
  
  # ----- Return ---------------------------------------------------------------
  
  bs_grid <- list(
    longitude = lon,
    latitude = lat,
    elevation = elevation,
    time = timeAxis,
    data = list(pm25 = parameter_data),
    model = model,
    modelRun = modelRun,
    deltaLon = lon[2] - lon[1],
    deltaLat = lat[2] - lat[1]
  )
  
  bs_grid <- structure(bs_grid, class = c("bs_grid", "list"))
  
  # Take out the trash
  gc()
  
  return(bs_grid)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  setModelDataDir("~/Data/Bluesky")
  
  dailyOutputDir <- "standard"
  model <- "PNW-4km"
  modelRun <- 20191009
  subDir <- "combined"
  parameter <- "pm25"
  xlim <- c(-118, -114)
  ylim <- c(45, 48)
  timesteps <- NULL
  download <- TRUE
  cleanup <- TRUE
  filePath <- NULL
  baseUrl <- "https://haze.airfire.org/bluesky-daily/output"
  quiet <- FALSE
  
  # Prepend
  timesteps <- seq(lubridate::ymd_h(2019100800), 
                   lubridate::ymd_h(2019101000), by = "hour")
  
  # Postpend
  timesteps <- seq(lubridate::ymd_h(2019101100), 
                   lubridate::ymd_h(2019101223), by = "hour")
  
  
}

