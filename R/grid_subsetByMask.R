#' @export
#' 
#' @title Subset a \emph{bs_grid} with with a gridMask 
#' 
#' @param bs_grid \emph{bs_grid} object.
#' @param gridMask Logical 2D matrix of the same dimensions as the grid data.
#' @param countryCodes Vector of country codes covered by the grid.
#' 
#' @description This function subsets the \code{bs_grid} object and 
#' restructures the data into a \emph{ws_monitor} that is compatible with 
#' \pkg{PWFSLSmoke} \code{monitor_~} functions.
#' 
#' @note The \code{countryCodes} parameter is provided because of the need
#' to perform spatial searches for during the conversion of grid cell locations into
#' \emph{ws_monitor} objects. Each location must have correct information for
#' \code{countryCode, stateCode, timezone} and these spatial searches can be
#' time consuming. Specifying the \code{countryCodes} covered by the grid
#' greatly speeds up these searches.
#' 
#' @return \emph{ws_monitor} object where each unique monitor represents a
#' single grid cell from \code{bs_grid}.
#' 
#' @seealso \link{grid_subsetByDistance} 
#' 
#' @examples
#' \dontrun{
#' # Next three lines required for bs_grid to ws_monitor conversion
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' # Now we can work as we normally would
#' setModelDataDir("~/Data/Bluesky")
#' bs <- bluesky_load(model = "PNW-1.33km", modelRun = 2019090900)
#' bs_monitors <- grid_subsetByDistance(bs, -121.017092, 46.901512, 
#'                                      radius = 10000)
#' 
#' distanceMatrix <- grid_distance(bs, -121.017092, 46.901512)
#' radius <- 20000
#' gridMask <- distanceMatrix <= radius
#' bs_monitors <- grid_subsetByMask(bs, gridMask)
#' PWFSLSmoke::monitor_staticmap(bs_monitors, cex = .5, pch = 15)
#' }

grid_subsetByMask <- function(
  bs_grid = NULL, 
  gridMask = NULL, 
  countryCodes = c('CA','US')
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(bs_grid)
  MazamaCoreUtils::stopIfNull(gridMask)
  MazamaCoreUtils::stopIfNull(countryCodes)
  
  if ( !("bs_grid" %in% class(bs_grid)) )
    stop("bs_grid object is not of class 'bs_grid'.")
  
  if ( ! length(dim(bs_grid$data[[1]])) == 3 ) {
    stop("Currently, only 3-D grids are supported.")
  }
  
  # Sanity check
  if ( length(bs_grid$longitude) != nrow(gridMask) || 
       length(bs_grid$latitude) != ncol(gridMask) ) {
    stop(paste0("bs_grid and gridMask have different dimensions."))
  }
  
  # ----- Create and apply the mask --------------------------------------------
  
  # How many cells in the final ws_monitor object
  cellCount <- sum(gridMask)
  
  # Create longitudes and latitudes associated with each grid cell
  allLons <- rep(bs_grid$longitude, times = length(bs_grid$latitude))
  allLats <- rep(bs_grid$latitude, each = length(bs_grid$longitude))
  
  lon2D <- matrix(
    allLons, 
    nrow = length(bs_grid$longitude), 
    ncol = length(bs_grid$latitude), 
    byrow = FALSE
  )
  lat2D <- matrix(
    allLats, 
    nrow = length(bs_grid$longitude), 
    ncol = length(bs_grid$latitude), 
    byrow = FALSE
  )
  
  # Apply mask to subset and 'unravel'
  latitude <- lat2D[gridMask]
  longitude <- lon2D[gridMask]
  
  # Construct a monitorID for each grid cell in our 'unraveled' vector
  monitorID <- paste(round(longitude, 2), round(latitude, 2), sep="_")
  
  # Calculate timezone (pick the middle index for the target lon and lat)
  targetLon <- longitude[cellCount/2]
  targetLat <- latitude[cellCount/2]
  timezone <-  
    MazamaSpatialUtils::getTimezone(targetLon, targetLat, useBuffering = TRUE)
  
  # Construct the meta dataframe
  meta <- data.frame(
    monitorID = monitorID,
    longitude = longitude,
    latitude = latitude,
    stringsAsFactors = FALSE
  )
  rownames(meta) <- meta$monitorID
  
  # Add missing elevation data
  meta$elevation <- as.numeric(NA)
  
  # Add timezone, stateCode and countryCode
  meta <- PWFSLSmoke::addMazamaMetadata(
    meta, 
    lonVar = "longitude", 
    latVar = "latitude",
    countryCodes = countryCodes
  )
  
  # Mask/unravel 3D pm25 array and put it back onto a 2D grid (cellID x time)
  unraveledPM25 <- bs_grid$data[["pm25"]][gridMask]
  
  # NOTE:  PM25 3D data are unraveled by X then Y and finally by T
  # NOTE:  We will put this into a matrix organized as col = gridCell and 
  # NOTE:  row = time (XY by T), then convert to a dataframe by including a 
  # NOTE:  datetime column, as we do with monitor data.
  
  pm25Matrix <- matrix(
    unraveledPM25, 
    nrow = length(bs_grid$time), 
    ncol = cellCount, 
    byrow = TRUE
  )
  pm25 <- data.frame(datetime = bs_grid$time, pm25Matrix)
  colnames(pm25) <- c("datetime", monitorID)
  
  # Create the 'ws_monitor'
  ws_monitor <- list(
    meta = meta, 
    data = pm25
  )
  structure(ws_monitor, class = c("ws_monitor", "list"))
  
  # Carry over all other singleton information (typically model run metadata)
  for ( name in names(bs_grid) ) {
    if ( class(bs_grid[[name]])[1] %in% c('character', 'numeric', 'POSIXct') ) {
      if ( length(bs_grid[[name]] == 1) ) {
        ws_monitor[[name]] <- bs_grid[[name]]
      }
    }
  }  
  
  return(structure(ws_monitor, class = c("ws_monitor", "list")))
  
}




