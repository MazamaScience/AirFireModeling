# Portland

library(MazamaSpatialUtils)
library(PWFSLSmoke)
library(AirFireModeling)

# Set data directories
MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
AirFireModeling::setModelDataDir('~/Data/BlueSky')

# Creating PWFSLSmoke ws_monitor objects requires:
PWFSLSmoke:::initializeMazamaSpatialUtils()

# ----- Monitor map -------------------------------------------------

# All monitors within 20 km of Portland
Portland_monitors <-
  monitor_load(
    startdate = 2019100900,
    enddate = 2019101123
  ) %>%
  monitor_subsetByDistance(
    longitude = -122.68,
    latitude = 45.52,
    radius = 20
  )

PWFSLSmoke::monitor_leaflet(
  Portland_monitors,
  slice = 32
)

# ----- Raster map -------------------------------------------------------------

# Get a raster for the Portland area

# Load model data
rasterList <- raster_load(
  model = "PNW-4km",
  modelRun = 2019100900,
  xlim = c(-123.08, -122.28),
  ylim = c(45.22, 45.92)
)

model_rasterBrick <- rasterList[[1]]

Portland_rasterBrick <-
  monitor_toRaster(
    Portland_monitors,
    model_rasterBrick
  )

# Compare the model and monitor rasters
raster_leaflet(
  raster = Portland_rasterBrick,
  index = 32,
  palette = "aqi",
  direction = 1
)

# ---- Converting back to monitor to test validity -----------------------------

reconstitutedMonitors <-
  raster_toMonitor(
    Portland_rasterBrick,
    longitude = -122.68,
    latitude = 45.52,
    radius = 20
  ) %>%
  monitor_reorder(dropMonitors = TRUE) # to get rid of monitors with all NA

# Plot origin monitors and reconstituted for comparison
monitor_timeseriesPlot(Portland_monitors)
monitor_timeseriesPlot(
  reconstitutedMonitors,
  pch=16,
  cex=0.6,
  col = 'red',
  add = TRUE
)

