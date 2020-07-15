library(MazamaSpatialUtils)
library(PWFSLSmoke)
PWFSLSmoke::initializeMazamaSpatialUtils(
  spatialDataDir = "~/Data/Spatial"
)
library(AirFireModeling)
library(dplyr)

# ----- Configurables ----------------------------------------------------------

longitude <- -119.1203
latitude <- 37.96207
radius <- 5 # km

# source ("~/Downloads/raster_leaflet_smo.R")

# Set the dataDir
dataDir <- "~/Data/BlueSky"
setModelDataDir(dataDir)

# Ingest the v2 file as a 'RasterBrick'
if (F) { # Loading the full thing takes a bazillion years to load
  fire_rasterBrick <- bluesky_load(
    localPath = file.path(dataDir, "smoke_dispersion_v2.nc"),
    modelName = "CANSAC-2km",
    modelRun = 2018072700,
    clean = FALSE)
} else {
  fire_rasterBrick <- bluesky_load(
    localPath = file.path(dataDir, "smoke_dispersion_v2.nc"),
    modelName = "CANSAC-2km",
    modelRun = 2018072700,
    xlim = c(-120, -118),
    ylim = c(39, 37),
    clean = FALSE)
}

# subset the rasterbrick
fire_rasterBrick_subset <- raster_subsetByDistance(
  fire_rasterBrick,
  longitude = longitude,
  latitude = latitude,
  radius = radius
)

fullRaster <- fire_rasterBrick[[329]] # 2018-08-05 05:00 UTC
subRaster <- fire_rasterBrick_subset[[329]] # 2018-08-05 05:00 UTC

# ----- Visual comparison ------------------------------------------------------

# look at a particular hour of data - full brick
raster_leaflet(
  fullRaster,
  index = 10,
  palette = "aqi",
  direction = 1,opacity = 0.6, maptype = "terrain", title = "PM2.5",
  breaks = c(0,12,50,100,200,250, Inf)
) %>%
leaflet::setView(fullRaster, lng = -119.1203, lat = 37.96207, zoom = 12)

# look at a particular hour of data - subset brick
raster_leaflet(
  subRaster,
  index = 10,
  palette = "aqi",
  direction = 1, opacity = 0.6, maptype = "terrain", title = "PM2.5",
  breaks = c(0,12,50,100,200,250, Inf)
) %>%
leaflet::setView(subRaster, lng = -119.1203, lat = 37.96207, zoom = 12)

# ----- Data comparison --------------------------------------------------------

fullValues <- matrix(
  data = raster::getValues(fullRaster),
  nrow = fullRaster@ncols,
  ncol = fullRaster@nrows
)

subValues <- matrix(
  data = raster::getValues(subRaster),
  nrow = subRaster@ncols,
  ncol = subRaster@nrows
)

# Print the raw data values for the rasters around Lee Vining
fullValuesFormatted <- round(t(apply(apply(fullValues[56:63, 68:73], 2, rev), 2, rev)))
subValuesFormatted <- round(t(apply(apply(subValues, 2, rev), 2, rev)))

# It looks like there aren't any value differences between the full raster and
# the subset raster. Do the differences only show up when visualizing them as
# leaflets?
