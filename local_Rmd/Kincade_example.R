# Kincade Fire

# ----- 108 hour facet plot ----------------------------------------------------

library(AirFireModeling)
setModelDataDir('~/Data/BlueSky')

# Kincade fire
rasterBrick <- raster_aggregate(
  model = "CANSAC-4km",
  firstModelRun = 2019102700,
  lastModelRun = 2019103100,
  xlim = c(-124, -121.5),
  ylim = c(37.5, 39)
)

raster_facet(
  rasterBrick,
  title = "Kincade Fire -- 2019102700 through 2019103100",
  ncol = 12,
  palette = 'Spectral',
  col_county = 'gray95',
  direction = -1,
  breaks = c(-Inf, 0, 12, 35, 55, 150, 250, 350, Inf)
)

# ----- 5 model run plot -------------------------------------------------------

library(AirFireModeling)
setModelDataDir('~/Data/BlueSky')

library(MazamaSpatialUtils)
PWFSLSmoke::initializeMazamaSpatialUtils()

# Load model data
rasterList <- raster_load(
  model = "CANSAC-4km",
  modelRun = c(2019102700, 2019102800, 2019102900, 2019103000, 2019103100),
  xlim = c(-124, -121.5),
  ylim = c(37.5, 39)
)

# Santa Rosa
raster_spaghettiPlot(
  rasterList,
  longitude = -122.71,
  latitude = 38.44,
  radius = 10, # km
  ylim = c(0, 1000)
)


