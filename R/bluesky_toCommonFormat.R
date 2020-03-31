#' @export
#'
#' @title Convert BlueSky NetCDF output to modernized NetCDF
#'
#' @param filePath Absolute path of file to be converted.
#' @param clean Logical specifying whether to remove the original netcdf file.
#'
#' @description Converts BlueSky model output from its original format to a more
#' modernized NetCDF format with dimension axes for longitude, latitude,
#' elevation and time. With default settings, output files renamed to
#' ~base~_v2.nc.
#'
#' @note Users will typically call \code{bluesky_load()} which in turn calls
#' this function.
#'
#' @return Absolute path of the converted NetCDF file.
#'
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#'
#' setModelDataDir('~/Data/BlueSky')
#' filePath <- bluesky_download(model = "PNW-4km", modelRun = 2019100900)
#' bluesky_toCommonFormat(filePath)
#' bluesky_downloaded()
#' }

bluesky_toCommonFormat <- function(
  filePath = NULL,
  clean = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(filePath)

  if ( !is.logical(clean) )
    clean <- TRUE

  # Check for v2 filePath
  if ( grepl(x = filePath, pattern = '.+_v2.nc$') )
    return(filePath)

  # ----- Open NetCDF file -----------------------------------------------------

  # Create old and new file paths
  rawFilePath <- filePath
  v2FilePath <- stringr::str_replace(rawFilePath, "\\.nc$", "_v2.nc")

  # Open nc file
  raw_nc <- ncdf4::nc_open(rawFilePath)

  # ----- Create latitude and longitude axes -----------------------------------

  # Current (index) values
  row <- raw_nc$dim$ROW$vals
  col <- raw_nc$dim$COL$vals
  lay <- raw_nc$dim$LAY$vals # LAYERS (height)

  # Useful information is found in the global attributes
  global_attributes <- ncdf4::ncatt_get(raw_nc, varid = 0) # varid=0 means 'global'

  # NOTE:  Use names(global_attributes) to see the names of the elements
  # NOTE:  contained in this list

  # NOTE:  global_attributes is of class 'list'
  # NOTE:  Access list elements with either 'listName[[objectName]]' or
  # NOTE:  'listName$objectName' notation

  XORIG <- global_attributes[["XORIG"]] # x origin
  YORIG <- global_attributes[["YORIG"]] # y origin
  XCENT <- global_attributes[["XCENT"]] # x center
  YCENT <- global_attributes[["YCENT"]] # y center
  ZLVLS <- global_attributes[["VGLVLS"]]

  # Now we have enough information about the domain to figure out W, E, S, N
  w <- XORIG
  e <- XORIG + 2 * abs(XCENT - XORIG)
  s <- YORIG
  n <- YORIG + 2 * (YCENT - YORIG)

  # Knowing the grid dimensions and the true edges, we can define legitimate
  # lat/lon dimensions
  lat <- seq(s, n, length.out = length(row))
  lon <- seq(w, e, length.out = length(col))
  lvl <- ZLVLS[1:length(lay)]

  # ----- Create time axis -----------------------------------------------------

  # Temporal information is stored in the 'TFLAG' variable
  tflag <- ncdf4::ncvar_get(raw_nc, "TFLAG")

  # NOTE:  'tflag' is a matrix object with two rows, one containing the year and
  # NOTE:  Julian day, the other containing time in HHMMSS format. We will paste
  # NOTE:  matrix elements together with 'paste()'.  The 'sprintf()' function is
  # NOTE:  useful for C-style string formatting. Here we use it to add leading
  # NOTE:  0s to create a string that is six characters long.

  time_str <- paste0(tflag[1,], sprintf(fmt = "%06d", tflag[2,]))

  # Create POSIXct time
  time <- tryCatch(
    expr = {
      MazamaCoreUtils::parseDatetime(time_str,
                                         timezone = "UTC",
                                         isJulian = TRUE)
    },
    warning = function(e) {
      warning(e)
      message('Error Parsing NetCDF data: Corrupt Download.')
      message('If problem persists, try deleting the NetCDF and downloading again.')
    }
  )

  # ----- Create new ncdf4 object ----------------------------------------------

  # NOTE:  The degenerate 'LAY' dimension disppears so that 'pm25' is now 2- or
  # NOTE:  3-D, not 3- or 4-D.

  # Get PM25 values
  pm25 <- ncdf4::ncvar_get(raw_nc, "PM25")

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
  nc <- ncdf4::nc_create(v2FilePath, pm25Var)

  # Put data into the newly defined variable
  ncdf4::ncvar_put(nc, pm25Var, pm25)

  # Close the file
  ncdf4::nc_close(nc)

  if (clean) {
    unlink(rawFilePath)
  }

  # ----- Return ---------------------------------------------------------------

  return(v2FilePath)

}
