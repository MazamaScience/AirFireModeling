#' @export
#' 
#' @title Subset a \emph{bs_grid} objectt by hydrologic unit code
#' 
#' @param bs_grid \emph{bs_grid} object.
#' @param HUC USGS hydrologic unit code.
#' 
#' @description This function subsets \code{bs_grid} and creates a new object
#' type \emph{ws_monitor}  that is compatible with \pkg{PWFSLSmoke} 
#' \code{monitor_~} functions.
#' 
#' @details Use of this function requires installation of the 
#' \code{WBDHU#.RData} files that can be downloaded with the 
#' \pkg{MazamaSpatialUtils} package. The appropriate data file matching the 
#' level of the \code{HUC} argument must be loaded as in the example.
#' 
#' @section USGS Watershed Boundary Dataset:
#' TODO:  Add description.
#' 
#' @return \emph{ws_monitor} object where each unique monitor represents a
#' single grid cell from \code{bs_grid}.
#' 
#' @seealso \link{grid_subsetByMask}
#' 
#' @examples
#' \dontrun{
#' # Next three lines required for bs_grid to ws_monitor conversion
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("WBDHU10")
#' 
#' longitude <- -123.801
#' latitude <- 47.704
#' WA10 <- MazamaSpatialUtils::subsetHUC(WBDHU10, allStateCodes = "WA")
#' HUC <- MazamaSpatialUtils::getHUC(longitude, latitude, "WA10")
#'
#' setModelDataDir("~/Data/Bluesky")
#' bs <- bluesky_load(model = "PNW-1.33km", modelRun = 2019090900)
#'
#' QueetsRiver <- grid_subsetByHUC(bs, HUC)
#' PWFSLSmoke::monitor_staticmap(QueetsRiver, cex = .5, pch = 15)
#' }

grid_subsetByHUC <- function(
  bs_grid, 
  HUC = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(bs_grid)
  MazamaCoreUtils::stopIfNull(HUC)
  
  if ( !("bs_grid" %in% class(bs_grid)) )
    stop("bs_grid object is not of class 'bs_grid'.")
  
  if ( ! length(dim(bs_grid$data[[1]])) == 3 ) {
    stop("Currently, only 3-D grids are supported.")
  }
  
  # HUC
  if ( (length(HUC) > 1) ) {
    stop("Need to specify a single HUC.")
  } else {
    HUC <- as.character(HUC)
  }
  
  # Sanity check
  SPDF_name <- paste0("WBDHU", stringr::str_length(HUC))
  if ( !exists(SPDF_name) ) {
    stop(paste0(
      "Please load ", SPDF_name, " with: ",
      "loadSpatialData(\"", SPDF_name, "\")"
    ))
  }
  
  # ----- Create and apply the mask --------------------------------------------
  
  # Get the individual HUC SpatialPolygonsDataFrame and its bbox
  SPDF <- get(SPDF_name)
  SPDF <- SPDF[SPDF$HUC == HUC,]
  SPDF_bbox <- sp::bbox(SPDF)
  
  # Subset model grid based on HUC bounds
  bs_grid <- grid_subset(bs_grid, xlim = SPDF_bbox[1,], ylim = SPDF_bbox[2,])
  
  # Create unraveled grids of lons and lats
  allLons <- rep(bs_grid$longitude, times = length(bs_grid$latitude))
  allLats <- rep(bs_grid$latitude, each = length(bs_grid$longitude))
  
  # Find the HUC at each location and test against the 'HUC' argument
  allHUCs <- MazamaSpatialUtils::getHUC(allLons, allLats, SPDF_name)
  
  # Create gridMask for the requested HUC
  allHUCs[is.na(allHUCs)] <- FALSE
  gridMask <- allHUCs == HUC
  
  # Create 2D gridded version of the mask
  gridMask <- matrix(
    gridMask, 
    nrow = length(bs_grid$longitude), 
    ncol = length(bs_grid$latitude), 
    byrow = FALSE
  )
  
  # NOTE:  Subsetting by mask "unravels" the gridded data and returns an object 
  # NOTE:  of type ws_monitor.
  
  # Apply gridMask to the bs_grid dataList
  ws_monitor <- grid_subsetByMask(bs_grid, gridMask)
  
  # ----- Return ---------------------------------------------------------------
  
  # Add 'HUC' to this list
  ws_monitor[["HUC"]] <- HUC
  
  # Return the ws_monitor data list
  return(ws_monitor)
  
}


# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaSpatialUtils)
  library(PWFSLSmoke)
  
  setSpatialDataDir("~/Data/Spatial")
  loadSpatialData("WBDHU10")
  
  setModelDataDir('~/Data/Bluesky')
  bs <- bluesky_load(model = "CANSAC-1.33km", 
                     modelRun = 2019100900,
                     subDir = "forecast")
  
  Mammoth_Lakes <- 
    monitor_load(20191008, 20191015) %>%
    monitor_subset(monitorIDs = "060510001_01")
  
  HUC <- MazamaSpatialUtils::getHUC(
    Mammoth_Lakes$meta$longitude, 
    Mammoth_Lakes$meta$latitude, 
    "WBDHU10"
  )

  drainage <- grid_subsetByHUC(bs, HUC)
  PWFSLSmoke::monitor_staticmap(drainage, cex = 1.0, pch = 15)
  
  monitor_timeseriesPlot(drainage, style = 'gnats', ylim=c(0,100), xpd=NA, shadedNight = TRUE)
  monitor_timeseriesPlot(Mammoth_Lakes, pch=16, col = "red", add = TRUE)
  
  addAQIStackedBar()


  library(AirMonitorPlots)
  
  gg <- 
    ggplot_pm25Timeseries(drainage) + 
    ggtitle("Mamoth Lakes") +
    #geom_pm25Points(shape = "square", alpha = .1) + 
    geom_boxplot(
      aes(group = datetime),
      outlier.size = 0.5,
      outlier.alpha = 0.2
    ) +
    scale_y_continuous(limits = c(0, 25))  
   
  print(gg) 
  
}