# ----- Testing belusky_aggregate() --------------------------------------------

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("NaturalEarthAdm1")

library(AirFireModeling)

setModelDataDir("~/Data/Bluesky")

# Aggregate a weeks worth of data
bs_grid <- bluesky_aggregate(
  model = "PNW-4km",
  firstModelRun = 20191007,
  lastModelRun = 20191013,
  subDir = "combined",
  chunk = 1
)

# Create a plot
bs_grid %>%
  grid_createMonitor(
    longitude = -116.5,
    latitude = 47.2,
    radius = 10000,
    monitorID = "Model data",
    FUN = quantile,
    probs = 0.90,
    na.rm = TRUE
  ) %>%
  PWFSLSmoke::monitor_timeseriesPlot(shadedNight = TRUE)


for ( modelRun in 20191007:20191013 ) {
  
  # Now add the full model time axis 
  bluesky_load(
    model = "PNW-4km",
    modelRun = modelRun,
    subDir = "combined"
  ) %>%
    grid_createMonitor(
      longitude = -116.5,
      latitude = 47.2,
      radius = 10000,
      monitorID = "Model data",
      FUN = quantile,
      probs = 0.90,
      na.rm = TRUE
    ) %>%
    PWFSLSmoke::monitor_timeseriesPlot(type='b', cex=0.5, pch=15, col="goldenrod", add = TRUE)
  
}

# ----- 20191007

# Now add the full model time axis 
bluesky_load(
  model = "PNW-4km",
  modelRun = 20191007,
  subDir = "combined"
) %>%
  grid_createMonitor(
    longitude = -116.5,
    latitude = 47.2,
    radius = 10000,
    monitorID = "Model data",
    FUN = quantile,
    probs = 0.90,
    na.rm = TRUE
  ) %>%
  PWFSLSmoke::monitor_timeseriesPlot(pch = 1, cex = 0.5, col = 'red', add = TRUE)

# TODO:  Looks like full model is shifted by one timestep

# ----- 20191008

# Now add the full model time axis 
bluesky_load(
  model = "PNW-4km",
  modelRun = 20191008,
  subDir = "combined"
) %>%
  grid_createMonitor(
    longitude = -116.5,
    latitude = 47.2,
    radius = 10000,
    monitorID = "Model data",
    FUN = quantile,
    probs = 0.90,
    na.rm = TRUE
  ) %>%
  PWFSLSmoke::monitor_timeseriesPlot(pch = 1, cex = 0.5, col = 'blue', add = TRUE)

# ----- 20191009

# Now add the full model time axis 
bluesky_load(
  model = "PNW-4km",
  modelRun = 20191009,
  subDir = "combined"
) %>%
  grid_createMonitor(
    longitude = -116.5,
    latitude = 47.2,
    radius = 10000,
    monitorID = "Model data",
    FUN = quantile,
    probs = 0.90,
    na.rm = TRUE
  ) %>%
  PWFSLSmoke::monitor_timeseriesPlot(pch = 1, cex = 0.5, col = 'orange', add = TRUE)

# ----- 20191010

# Now add the full model time axis 
bluesky_load(
  model = "PNW-4km",
  modelRun = 20191010,
  subDir = "combined"
) %>%
  grid_createMonitor(
    longitude = -116.5,
    latitude = 47.2,
    radius = 10000,
    monitorID = "Model data",
    FUN = quantile,
    probs = 0.90,
    na.rm = TRUE
  ) %>%
  PWFSLSmoke::monitor_timeseriesPlot(pch = 1, cex = 0.5, col = 'cyan', add = TRUE)


