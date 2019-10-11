# #' @keywords bluesky, ws_grid
# #' @export
# #' @title Read BlueSky NetCDF File and Return Contents in a List
# #' @param dailyOutputDir BlueSky web directory
# #' @param model model identifier
# #' @param modelRun date code as "YYYYMMDDHH" (integer or character)
# #' @param subDir subdirectory path containing netcdf data
# #' @param param parameter name
# #' @param download logical specifying whether to download and convert data if it is not found locally
# #' @param cleanup Logical specifying whether to remove the original files
# #' @param file Absolute path of the local bluesky .nc file for the special occasions. (ex: 4D bluesky model)
# #' @description Reads in a "_v2" BlueSky output NetCDF file and ingests the data, returning it 
# #' in a list of type \code{ws_grid}.
# #' @details Data are searched for in the package internal \code{dataDir} which can be set with
# #' \code{setModelDataDir()}.
# #' 
# #' #' Available model identifiers include the following:
# #' \itemize{
# #'   \item{CANSAC-2km}
# #'   \item{CANSAC-6km}
# #'   \item{NAM36-0.08deg}
# #'   \item{NAM84-0.08deg}
# #'   \item{NAM84-0.15deg}
# #'   \item{PNW-1.33km}
# #'   \item{PNW-4km}
# #' }
# #' 
# #' When \code{modelRun=NULL} today's date will be used.
# #' @return List of class \code{ws_grid} with the following elements:
# #' \itemize{
# #'   \item{lon -- 1D vector of longitudes (X)}
# #'   \item{lat -- 1D vector of latitudes (Y)}
# #'   \item{time -- 1D vector of POSIXct times (T)}
# #'   \item{pm25 -- 3D array of PM2.5 values (X-Y-T)}
# #'   \item{model -- character name of model used to generate the data}
# #'   \item{modelRun -- character date + hour when the model was run}
# #'   \item{deltaLon -- longitude axis grid cell spacing}
# #'   \item{deltaLat -- latitude axis grid cell spacing}
# #' }
# #' 
# #' @note
# #' In May of 2016, the directory structure for model output changed. Previously,
# #' bluesky dailiy output directories were of the following nature:
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
# #' @seealso \link{setModelDataDir}
# #' @seealso \link{bluesky_2v2}
# #' @seealso \link{bluesky_download}
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' bs <- bluesky_load(model="PNW-1.33km", modelRun=2016091000)
# #' }

# bluesky_load <- function(dailyOutputDir='standard',
#                          model="PNW-1.33km",
#                          modelRun=NULL,
#                          subDir='combined',
#                          param='pm25',
#                          download=TRUE, cleanup=TRUE, file=NULL) {
  
#   # NOTE:  The suffix could be a parameter but why let people create a mess with different suffixes?
#   suffix <- '_v2'
  
#   if ( is.null(modelRun) ) {
#     modelRun <- paste0(format(Sys.Date(), '%Y%m%d'), '00')
#   } else {
#     # Make sure we end up with YYYYmmddHH
#     modelRun <- as.character(modelRun)
#     length <- stringr::str_length(modelRun)[1]
#     if ( length == 8 ) {
#       modelRun <- paste0(modelRun,'00')
#     }
#   }
  
#   # Create filePath
#   if ( is.null(file) ) {
#     filePath <- paste0(getModelDataDir(),'/',model,'_',modelRun,suffix,'.nc')
#   } else {
#     newPath <- bluesky_2v2(file = file, cleanup = FALSE)
#     filePath <- newPath
#   }
  
#   # Open or download
#   if ( file.exists(filePath) ) {
    
#     nc <- ncdf4::nc_open(filePath)
    
#   } else {
    
#     if ( download ) {
      
#       # Extract information from filename
#       fileName <- basename(filePath)
#       matchMatrix <- stringr::str_match(fileName,'(.+)_([0-9]+)(_.+).nc')
#       model <- matchMatrix[,2]
#       modelRun <- matchMatrix[,3]
#       suffix <- matchMatrix[,4]
      
#       # Download and convert data
#       oldFilePath <- bluesky_download(dailyOutputDir ,model, modelRun, subDir)
#       newFilePath <- bluesky_2v2(oldFilePath,suffix, cleanup)
      
#       # Sanity check
#       if (newFilePath != filePath) {
#         stop(paste0('bluesky_load: filepath mismatch: filePath="',filePath,
#                     '" but newFilePath="',newFilePath,'".'))
#       }
      
#       # Open file
#       nc <- ncdf4::nc_open(newFilePath)      
      
#     } else {
      
#       stop(paste0('bluesky_load: file "',fileName,'" not found and download=FALSE.'))
      
#     }
    
#   }
  
#   # TODO:  Handle errors
  
#   data <- list()
    
#   # NOTE:  All netcdf variables are returned as class "array" and must be 
#   # NOTE:  convnerted to numeric or POSIXct as needed.
#   lon <- as.numeric(ncdf4::ncvar_get(nc, "lon"))
#   lat <- as.numeric(ncdf4::ncvar_get(nc, "lat"))
#   elevation <- as.numeric(ncdf4::ncvar_get(nc, "elevation"))
#   time <- ncdf4::ncvar_get(nc, "time")
#   time <- as.POSIXct(time, origin="1970-1-1", tz="UTC")
  
#   # NOTE:  Sometimes the time variable has no data. In this case we create an hourly
#   # NOTE:  axis starting at modelRun.
#   if ( any(is.na(time)) ) {
#     time <- seq(parseDatetime(modelRun), by="hours", length.out=length(time))
#   }
  
#   # TODO:  Handle parameters other than "PM25"
#   pm25 <- ncdf4::ncvar_get(nc, "PM25")
#   if ( param == 'pm25' ) {
#     data$pm25 <- pm25
#   } else {
#     stop(paste0('bluesky_load: Parameter "',param,'" is not handled yet.'))
#   }
  
#   # Close the file now that everything is in memory
#   ncdf4::nc_close(nc)
  
#   deltaLon <- lon[2] - lon[1]
#   deltaLat <- lat[2] - lat[1]
  
#   # Return
#   return(structure(list(longitude=lon,
#                         latitude=lat,
#                         elevation=elevation,
#                         time=time,
#                         data=data,
#                         model=model,
#                         modelRun=modelRun,
#                         deltaLon=deltaLon,
#                         deltaLat=deltaLat),
#                    class = c("ws_grid", "list")))
# }

