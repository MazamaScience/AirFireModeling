# Helping Susan O'Neill work with a 2.8G file

# ----- Setup ------------------------------------------------------------------

library(MazamaSpatialUtils)
library(PWFSLSmoke)
PWFSLSmoke::initializeMazamaSpatialUtils(
  spatialDataDir = "~/Data/Spatial"
)

# Set up AirFire Modeling (version 0.2.14)
library(AirFireModeling)
setModelDataDir("~/Data/BlueSkky")

# ----- Configurables ----------------------------------------------------------

# Convenient shorhand
dataDir <- file.path(
  "/Users/jonathan/Data/Susan_ONeill/",
  "Output_20180727_dispersion_MendocinoC.1/data"
)

# Target location
longitude <- -119.5869
latitude <- 37.7846
radius <- 2 # km

# xlim and ylim to reduce amount of data read in
halfWidth <- 0.2
xlim <- c(longitude - halfWidth, longitude + halfWidth)
ylim <- c(latitude - halfWidth, latitude + halfWidth)

# ----- Load data --------------------------------------------------------------

# NOTE:  bluesky_load() will automatically run bluesky_toCommonFormat() if the
# NOTE:  v2 version of the file is not found

# Ingest the v2 file as a 'RasterBrick'
mendo_rasterBrick <- bluesky_load(
  localPath = file.path(dataDir, "smoke_dispersion_v2.nc"),
  xlim = xlim,
  ylim = ylim,
  clean = FALSE
)

# How big is it?
dim(mendo_rasterBrick)
pryr::object_size(mendo_rasterBrick)

# ----- Get to work! -----------------------------------------------------------

# Convert to 'ws_monitor'
mendo_monitor <- raster_toMonitor(
  mendo_rasterBrick,
  longitude = longitude,
  latitude = latitude,
  radius = radius
)

# Plot it
monitor_timeseriesPlot(
  mendo_monitor,
  shadedNight = TRUE
)

# TODO:  Jon -- fix raster_spaghettiPlot to work with a rasterBrick

# Now try a "spaghetti plot"
# Just use the first 168 time steps (1 week)
raster_spaghettiPlot(
  raster = list("MendocinoC.1" = mendo_rasterBrick[[1:168]]),
  ###raster = mendo_rasterBrick,
  longitude = longitude,
  latitude = latitude,
  radius = radius,
  localTime = TRUE
)

