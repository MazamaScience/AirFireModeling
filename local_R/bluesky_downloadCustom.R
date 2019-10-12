#' @keywords bluesky
#' @export
#' @title Download Custom BlueSky Model Data from PWFSL
#' @param folderUrl URL where the BlueSky output .nc file resides (excluding filename)
#' @param baseFileName base name for downloaded file (excluding .nc extension and _v2 if applicable).
#' If left empty, temporary name is assigned based on system date/time (e.g. "temp_20170321_130143.nc")
#' @param cleanup Logical specifying whether to remove the original files
#' @description Downloads a copy of the specified file to the package data directory. 
#' This file is then converted into a more modernized format by bluesky2v2().
#' 
#' @return file path of downloaded data
#' @seealso \link{setModelDataDir}
#' @seealso \link{bluesky_2v2}
#' @seealso \link{bluesky_load}
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' folderUrl <- "https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/PNW-4km-fixEMISS_2015/2015081100_smoldFrac10/data/"
#' baseFileName <- "PNW-4km-fixEMISS_2015_smoldFract10_2015081100"
#' bluesky_downloadCustom(folderUrl=folderUrl, baseFileName=baseFileName)
#' }

bluesky_downloadCustom <- function(folderUrl=NULL, baseFileName=NULL, cleanup=TRUE) {
  
  if ( is.null(folderUrl) ) {
    stop("Please provide a base URL.")
  }
  
  fileURL <- paste0(folderUrl,"smoke_dispersion.nc")
  
  if ( is.null(baseFileName) ) {
    baseFileName <- paste0("temp_",format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  filePath <- paste0(getModelDataDir(),'/',baseFileName,'.nc')
  
  # Only download data if it doesn't already exist
  if ( file.exists(filePath) ) {
    filePath <- bluesky_2v2(file=filePath, cleanup=cleanup)
  }
  
  # Only download data if it doesn't already exist
  if ( !file.exists(paste0(getModelDataDir(),'/',baseFileName,'_v2.nc')) ) {
    
    result <- try( utils::download.file(url=fileURL, destfile=filePath) )
    
    # Handle errors
    if ( class(result) != "try-error" ) {
      
      # do nothing on success
      
    } else {
      
      err_msg <- geterrmessage()
      if ( stringr::str_detect(err_msg,"cannot open destfile") ) {
        # do nothing, already generated error message is sufficient
      } else if ( stringr::str_detect(err_msg,"404 Not Found") ) {
        # do nothing, already generated error message is sufficient
      } else {
        # do nothing, already generated error message is sufficient
      }
      
    }
    
  }
  
  # Convert to version 2
  newFilePath <- bluesky_2v2(file=filePath, cleanup=cleanup)
  
  # return
  return(newFilePath)
  
}
