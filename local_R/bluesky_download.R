# #' @keywords bluesky
# #' @export
# #' @title Download BlueSky Model Data from PWFSL
# #' @param dailyOutputDir BlueSky web directory
# #' @param model model identifier
# #' @param modelRun date code as "YYYYMMDDHH"
# #' @param subDir subdirectory path containing netcdf data
# #' @param baseUrl base URL for BlueSky output
# #' @description Downloads a copy of the specified BlueSky model run to the package data directory. 
# #' This file can then be converted into a more modernized format by bluesky2v2().
# #'
# #' Available model identifiers include the following:
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
# #' 
# #' Users will typically call bluesky_load() which in turn calls this function.
# #' 
# #' @note
# #' In May of 2016, the directory structure for model output changed. Previously,
# #' bluesky daily output directories were of the following nature:
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
# #' @return file path of downloaded data
# #' @seealso \link{setModelDataDir}
# #' @seealso \link{bluesky_2v2}
# #' @seealso \link{bluesky_load}
# #' @examples
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' bluesky_download(model="PNW-4km", modelRun=2016092200)
# #' }

# bluesky_download <- function(dailyOutputDir="standard",
#                              model="PNW-1.33km",
#                              modelRun=NULL,
#                              subDir='combined',
#                              baseUrl="https://haze.airfire.org/bluesky-daily/output") {
  
#   # Use today's date if no modelRun was specified
#   if ( is.null(modelRun) ) {
#     stop("Required parameter 'modelRun' is missing")
#   } else {
#     modelRun <- as.character(modelRun)
#   }
  
#   if ( length(model) > 1 || length(modelRun) > 1 ) {
#     warning("'model' or 'modelRun' parameter has multiple values -- first value being used.")
#     model <- model[1]
#     modelRun <- modelRun[1]  
#   }
    
#   # Create the URL and local name
#   if ( is.null(subDir) ) {
#     fileURL <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/data/smoke_dispersion.nc")
#   } else {
#     fileURL <- paste0(baseUrl, "/", dailyOutputDir, "/", model, "/", modelRun, "/", subDir, "/data/smoke_dispersion.nc")
#   }
#   fileName <- paste0(model,"_",modelRun,".nc")
#   filePath <- paste0(getModelDataDir(),'/',fileName)
  
#   # Only download data if it doesn't already exist
#   if ( !file.exists(filePath) ) {
    
#     result <- try( utils::download.file(url=fileURL, destfile=filePath) )
    
#     # Handle errors
#     if ( class(result) != "try-error" ) {
      
#       # do nothing on success
      
#     } else {
      
#       err_msg <- geterrmessage()
#       if ( stringr::str_detect(err_msg,"cannot open destfile") ) {
#         # do nothing, already generated error message is sufficient
#       } else if ( stringr::str_detect(err_msg,"404 Not Found") ) {
#         if ( is.null(subDir) && as.numeric(modelRun) > 2016050000 ) {
#           warning("For modelRuns after April, 2016 you must specify the subDir parameter, e.g. subDir='forecast'." )
#         }
#       } else {
#         # do nothing, already generated error message is sufficient
#       }
      
#     }
    
#   }
  
#   # return
#   return(filePath)
  
# }
