# #' @keywords bluesky
# #' @export
# #' @title Convert BlueSky NetCDF output to Modernized NetCDF
# #' @param file absolute path of file to be converted
# #' @param suffix character string used to identify 'v2' files
# #' @param cleanup logical specifying whether to remove the original files
# #' @description Converts BlueSky model output from its original format to a more modernized
# #' NetCDF format with dimension axes for longitude, latitude, elevation and time. With default settings,
# #' output files renamed to ~base~_v2.nc.
# #' @note Users will typically call bluesky_load() which in turn calls this function.
# #' @return Absolute path of converted NetCDF file.
# #' @seealso \link{setModelDataDir}
# #' @seealso \link{bluesky_download}
# #' @seealso \link{bluesky_load}
# #' @examples 
# #' \dontrun{
# #' setModelDataDir('~/Data/Bluesky')
# #' bluesky_download(model="PNW-4km", modelRun=2016092200)
# #' bluesky_2v2("~/Data/Bluesky/PNW-4km_2016092200.nc")
# #' }

# bluesky_2v2 <- function(file, suffix="_v2", cleanup=TRUE) {
  
#   # Create old and new file paths
#   oldFilePath <- file
#   newFilePath <- stringr::str_replace(oldFilePath, "\\.nc$", paste0(suffix,".nc"))
    
#   # open nc file
#   old_nc <- ncdf4::nc_open(oldFilePath)
  
#   # ----- Create latitude and longitude axes -----------------------------------
  
#   # Current (index) values
#   row <- old_nc$dim$ROW$vals
#   col <- old_nc$dim$COL$vals
#   lay <- old_nc$dim$LAY$vals
  
#   # Useful information is found in the global attributes
#   globalAttributes <- ncdf4::ncatt_get(old_nc, varid=0) # varid=0 means 'global'
  
#   # Use names(globalAttributes) to see the names of the elements contained in this list
  
#   # NOTE:  globalAttributes is of class 'list'
#   # NOTE:  Access list elements with either 'listName[[objectName]]' or 'listName$objectName' notation
  
#   XORIG <- globalAttributes[["XORIG"]] # x origin
#   YORIG <- globalAttributes[["YORIG"]] # y origin
#   XCENT <- globalAttributes[["XCENT"]] # x center
#   YCENT <- globalAttributes[["YCENT"]] # y center
#   ZLVLS <- globalAttributes[["VGLVLS"]]
  
#   # Now we have enough information about the domain to figure out the n, e, s, w corners
#   w <- XORIG
#   e <- XORIG + 2 * abs(XCENT - XORIG)
#   s <- YORIG
#   n <- YORIG + 2 * (YCENT - YORIG)
  
#   # Knowing the grid dimensions and the true corners we can define legitimate lat/lon dimensions
#   lat <- seq(s, n, length.out=length(row))
#   lon <- seq(w, e, length.out=length(col))
#   lvl <- ZLVLS[1:length(lay)]
  
#   # ----- Create time axis -----------------------------------------------------
  
#   # Temporal information is stored in the 'TFLAG' variable
#   tflag <- ncdf4::ncvar_get(old_nc, "TFLAG")
  
#   # NOTE:  'TFLAG' is a matrix object with two rows, one containing the year and Julian day, 
#   # NOTE:  the other containing time in HHMMSS format. We will paste matrix elements together
#   # NOTE:  with 'paste()'.  The 'sprintf()' function is useful for C-style string formatting.
#   # NOTE:  Here we use it to add leading 0s to create a string that is six characters long.
#   time_str <- paste0(tflag[1,], sprintf(fmt="%06d", tflag[2,]))
  
#   # We use 'strptime()' to convert our character index to a "POSIXct" value.
#   time <- strptime(x=time_str, format="%Y%j%H%M%S", tz="GMT")
  
#   # ----- Create new ncdf4 object ----------------------------------------------
  
#   # Get PM25 values
#   # NOTE:  The degenerate 'LAY' dimension disppears so that 'pm25' is now 3D, not 4D. 
#   pm25 <- ncdf4::ncvar_get(old_nc, "PM25")
  
#   # Convert time to numeric value for storing purposes
#   numericTime <- as.numeric(time)
  
#   # Define dimensions
#   latDim <- ncdf4::ncdim_def("lat", "Degrees North", lat) 
#   lonDim <- ncdf4::ncdim_def("lon", "Degrees East", lon)
#   lvlDim <- ncdf4::ncdim_def("elevation", "Meters from sea level", lvl)
#   timeDim <- ncdf4::ncdim_def("time", "seconds from 1970-1-1", numericTime)
  
#   # Define variables
#   pm25Var <- ncdf4::ncvar_def(name="PM25", units="ug/m^3", dim=list(lonDim, latDim, lvlDim, timeDim), missval=-1e30)
  
#   # Create a new netcdf file 
#   new_nc <- ncdf4::nc_create(newFilePath, pm25Var)
  
#   # Put data into the newly defined variable 
#   ncdf4::ncvar_put(new_nc, pm25Var, pm25)
  
#   # Close the file
#   ncdf4::nc_close(new_nc)
  
#   if (cleanup) {
#     unlink(oldFilePath)
#   }
  
#   # Return
#   return(newFilePath)
  
# }
