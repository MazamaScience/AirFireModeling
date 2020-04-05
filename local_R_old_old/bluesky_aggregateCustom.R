#' @keywords bluesky, bs_grid
#' @export
#' @title Aggregate Multiple Custom Bluesky Models into a Single bs_grid Object
#' @param model The model of the bluesky output. 
#' @param fileNames A vector of filenames to aggregate.
#' @param spinUpHrs Number of hours to drop from the start of each model run.
#' @param param parameter name
#' @param dataDir directory where files are saved
#' @description Combines multiple custom bluesky gridded output data and produce a single bluesky object. 
#' User passes in the model type (for identification purposes only), a vector of filenames, and the number
#' of hours to ignore from the start of each model run (for model spin-up).
#' 
#' @return bs_grid object where multiple bluesky models are aggregated 
#' @examples
#' \dontrun{
#' setModelDataDir('~/Data/Bluesky')
#' bluesky_downloadCustom('https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/PNW-4km-fixEMISS_2015/2015081100_smoldfrac10/data/','PNW-4km-fixEMISS_2015_smoldFract10_2015081100')
#' bluesky_downloadCustom('https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/PNW-4km-fixEMISS_2015/2015081800_smoldFrac10/data/','PNW-4km-fixEMISS_2015_smoldFract10_2015081800')
#' bluesky_downloadCustom('https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/PNW-4km-fixEMISS_2015/2015082500_smoldFrac10/data/','PNW-4km-fixEMISS_2015_smoldFract10_2015082500')
#' fileNames <- c('PNW-4km-fixEMISS_2015_smoldFract10_2015081100_v2.nc',
#' 'PNW-4km-fixEMISS_2015_smoldFract10_2015081800_v2.nc',
#' 'PNW-4km-fixEMISS_2015_smoldFract10_2015082500_v2.nc')
#' bs <- bluesky_aggregateCustom(model="PNW-4km", fileNames=fileNames)
#' 
#' }

bluesky_aggregateCustom <- function(model=NULL,
                               fileNames=NULL,
                               spinUpHrs=47,
                               param='pm25',
                               dataDir=getModelDataDir()) {
  
  # set up placeholders
  bsList <- list()
  startTime <- c()
  endTime <- c()
  modelRuns <- c()
  
  checkSums <- list()
  
  # For each filename provided, import the data and save as component in list (i.e. bsList)
  for (f in 1:length(fileNames)) {
    
    # load data and save as component in list
    fullPath <- paste0(dataDir,'/',fileNames[f])
    bsTemp <- bluesky_loadCustom(filePath=fullPath,model=model)
    bsList[[bsTemp$modelRun]] <- bsTemp
    
    # pull out min and max timestamps for new time axis
    startTime <- min(startTime,bsTemp$time[1])
    endTime <- max(endTime,bsTemp$time[length(bsTemp$time)])
    
    # pull out model run names
    modelRuns <- c(modelRuns,bsTemp$modelRun)
    
    tempCheckDF <- data.frame(time=bsTemp$time,sums=apply(bsList[[bsTemp$modelRun]]$data$pm25,MARGIN = 3,sum))
    checkSums[[bsTemp$modelRun]] <- tempCheckDF[-(1:spinUpHrs),]
    
  }
  
  # Extract the constant information such as longitude and latitude from the first object from the list
  # Constract the empty list of pm25 and timeSets for aggregating them later
  lon <- bsList[[1]]$longitude
  lat <- bsList[[1]]$latitude
  elevation <- bsList[[1]]$elevation
  model <- bsList[[1]]$model
  modelRun <- paste(min(modelRuns), max(modelRuns), sep="_")
  deltaLon <- bsList[[1]]$deltaLon
  deltaLat <- bsList[[1]]$deltaLat
  
  # convert min and max timestamps back to POSIXct and create new time axis
  startTime <- as.POSIXct(startTime,origin=lubridate::origin,tz = "UTC")
  endTime <- as.POSIXct(endTime,origin=lubridate::origin,tz = "UTC")
  newTimeAxis <- seq(startTime,endTime,by="1 hour")
  newTimeAxis <- newTimeAxis[-(1:spinUpHrs)]
  
  # initialize aggregated pm25 data placeholder
  dataDim <- dim(bsList[[1]]$data[[param]])[1:2]
  pm25 <- array(NA,dim=c(dataDim,length(newTimeAxis)))
  
  # plug in individual model run data into combined data structure
  for (m in order(modelRuns)) {

    # verify consistent dimensions
    if (any(dataDim != dim(bsTemp$data[[param]])[1:2])) {
      stop("Inconsitent data dimensions.")
    }
    
    # define timestamps and pm25 data for current model run
    tempTime <- bsList[[modelRuns[m]]]$time
    tempPm25 <- bsList[[modelRuns[m]]]$data$pm25
    
    # run time and pm25 data after spin-up hours removed
    tempTime <- tempTime[-(1:spinUpHrs)]
    tempPm25 <- tempPm25[,,-(1:spinUpHrs)]
    
    # fill the pm25 array with data from the current model run
    indexesToFill <- which(newTimeAxis %in% tempTime)
    pm25[,,indexesToFill] <- tempPm25
    
  }
  
  # ----- Create and return the bs_grid object -------------------------------------------------------
  
  return(structure(list(longitude=lon,
                        latitude=lat,
                        elevation=elevation,
                        time=newTimeAxis,
                        data=list(pm25=pm25),
                        model=model,
                        modelRun=modelRun,
                        deltaLon=deltaLon,
                        deltaLat=deltaLat),
                   class=c("bs_grid", "list")))
  
}