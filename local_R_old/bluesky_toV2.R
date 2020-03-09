#' @export
#'
#' @title Convert BlueSky NetCDF output to modernized NetCDF
#'
#' @param filePath Absolute path of file to be converted.
#' @param cleanup Logical specifying whether to remove the original netcdf file.
#'
#' @description Converts BlueSky model output from its original format to a more
#' modernized NetCDF format with dimension axes for longitude, latitude,
#' elevation and time. With default settings, output files renamed to
#' ~base~_V2.nc.
#'
#' @note Users will typically call \code{bluesky_load()} which in turn calls
#' this function.
#'
#' @return Absolute path of the converted NetCDF file.
#'
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' filePath <- bluesky_download(model = "PNW-4km", modelRun = 2019100900)
#' bluesky_toV2(filePath)
#' }

bluesky_toV2 <- function(
  filePath = NULL,
  cleanup = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(filePath)

  if ( !is.logical(cleanup) )
    cleanup <- TRUE

  # ----- Open NetCDF file -----------------------------------------------------

  # Create old and new file paths
  oldFilePath <- filePath
  newFilePath <- stringr::str_replace(oldFilePath, "\\.nc$", "_V2.nc")

  # open nc file
  old_nc <- ncdf4::nc_open(oldFilePath)

  # ----- Create latitude and longitude axes -----------------------------------

  # Current (index) values
  row <- old_nc$dim$ROW$vals
  col <- old_nc$dim$COL$vals
  lay <- old_nc$dim$LAY$vals # LAYERS (height)

  # Useful information is found in the global attributes
  globalAttributes <- ncdf4::ncatt_get(old_nc, varid=0) # varid=0 means 'global'

  # NOTE:  Use names(globalAttributes) to see the names of the elements
  # NOTE:  contained in this list

  # NOTE:  globalAttributes is of class 'list'
  # NOTE:  Access list elements with either 'listName[[objectName]]' or
  # NOTE:  'listName$objectName' notation

  XORIG <- globalAttributes[["XORIG"]] # x origin
  YORIG <- globalAttributes[["YORIG"]] # y origin
  XCENT <- globalAttributes[["XCENT"]] # x center
  YCENT <- globalAttributes[["YCENT"]] # y center
  ZLVLS <- globalAttributes[["VGLVLS"]]

  # Now we have enough information about the domain to figure out W, E, S, N
  w <- XORIG
  e <- XORIG + 2 * abs(XCENT - XORIG)
  s <- YORIG
  n <- YORIG + 2 * (YCENT - YORIG)

  # Knowing the grid dimensions and the true edges, we can define legitimate
  # lat/lon dimensions
  lat <- seq(s, n, length.out=length(row))
  lon <- seq(w, e, length.out=length(col))
  lvl <- ZLVLS[1:length(lay)]

  # ----- Create time axis -----------------------------------------------------

  # Temporal information is stored in the 'TFLAG' variable
  tflag <- ncdf4::ncvar_get(old_nc, "TFLAG")

  # NOTE:  'tflag' is a matrix object with two rows, one containing the year and
  # NOTE:  Julian day, the other containing time in HHMMSS format. We will paste
  # NOTE:  matrix elements together with 'paste()'.  The 'sprintf()' function is
  # NOTE:  useful for C-style string formatting. Here we use it to add leading
  # NOTE:  0s to create a string that is six characters long.

  time_str <- paste0(tflag[1,], sprintf(fmt="%06d", tflag[2,]))

  # Create POSIXct time
  time <- MazamaCoreUtils::parseDatetime(time_str,
                                         timezone = "UTC",
                                         isJulian = TRUE)

  # ----- Create new ncdf4 object ----------------------------------------------

  # NOTE:  The degenerate 'LAY' dimension disppears so that 'pm25' is now 2- or
  # NOTE:  3-D, not 3- or 4-D.

  # Get PM25 values
  pm25 <- ncdf4::ncvar_get(old_nc, "PM25")

  # Convert time to numeric value for storing purposes
  numericTime <- as.numeric(time)

  # Define dimensions
  latDim <- ncdf4::ncdim_def("lat", "Degrees North", lat)
  lonDim <- ncdf4::ncdim_def("lon", "Degrees East", lon)
  lvlDim <- ncdf4::ncdim_def("elevation", "Meters from sea level", lvl)
  timeDim <- ncdf4::ncdim_def("time", "seconds from 1970-1-1", numericTime)

  # Define variables
  pm25Var <- ncdf4::ncvar_def(
    name = "PM25",
    units = "ug/m^3",
    dim = list(lonDim, latDim, lvlDim, timeDim),
    missval = -1e30
  )

  # Create a new netcdf file
  new_nc <- ncdf4::nc_create(newFilePath, pm25Var)

  # Put data into the newly defined variable
  ncdf4::ncvar_put(new_nc, pm25Var, pm25)

  # Close the file
  ncdf4::nc_close(new_nc)

  if (cleanup) {
    unlink(oldFilePath)
  }

  # ----- Return ---------------------------------------------------------------

  return(newFilePath)

}
