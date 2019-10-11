# #' @keywords bluesky, ws_grid
# #' @export
# #' @title Read BlueSky NetCDF File and Return Contents in a List
# #' @param filePath file path (including file name) of model data file
# #' @param model model identifier (for metadata only)
# #' @param param parameter name (currently only 'pm25' is supported)
# #' @description Reads in a "_v2" BlueSky output NetCDF file and ingests the data, returning it 
# #' in a list of type \code{ws_grid}. Unlike the standard bluesky_load(), the data must already
# #' be downloaded prior to running this function.
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
# #' @return List of class \code{ws_grid} with the following elements:
# #' \itemize{
# #'   \item{longitude -- 1D vector of longitudes (X)}
# #'   \item{latitude -- 1D vector of latitudes (Y)}
# #'   \item{time -- 1D vector of POSIXct times (T)}
# #'   \item{pm25 -- 3D array of PM2.5 values (X-Y-T)}
# #'   \item{model -- character name of model used to generate the data}
# #'   \item{modelRun -- character date + hour when the model was run}
# #'   \item{deltaLon -- longitude axis grid cell spacing}
# #'   \item{deltaLat -- latitude axis grid cell spacing}
# #' }
# #' 
# #' @seealso \link{setModelDataDir}
# #' @seealso \link{bluesky_2v2}
# #' @seealso \link{bluesky_download}
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' folderUrl <- "https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/PNW-4km-fixEMISS_2015/2015081800_smoldFrac10/data/"
# #' baseFileName <- "PNW-4km-fixEMISS_2015_smoldFract10_2015081800"
# #' bluesky_downloadCustom(folderUrl=folderUrl, baseFileName=baseFileName)
# #' filePath <- paste0(getModelDataDir(),"/",baseFileName,"_v2.nc")
# #' bs <- bluesky_loadCustom(filePath,model="PNW-4km")
# #' }
  
# bluesky_loadCustom <- function(filePath=NULL,
#                                model=NULL,
#                                param='pm25') {
  
#   # ensure model is entered
#   if( is.null(model) ) {
#     stop("Please enter model name (e.g. PNW-4km)")
#   }
  
#   # NOTE:  The suffix could be a parameter but why let people create a mess with different suffixes?
#   suffix <- '_v2'
  
#   # Attempt to open file
#   if ( file.exists(filePath) ) {
#     nc <- ncdf4::nc_open(filePath)
#   } else {
#       warning(paste0('bluesky_load: file not found.'))
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
  
#   # Model run
#   modelRun <- format(time[1]-lubridate::hours(1),"%Y%m%d%H")
  
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

