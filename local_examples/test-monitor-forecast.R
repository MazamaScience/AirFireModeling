# ----- Testing monitor forecast combo -----------------------------------------

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("NaturalEarthAdm1")

library(PWFSLSmoke)
library(AirFireModeling)
setModelDataDir("~/Data/Bluesky")

Pollock_Pines <-
  monitor_load(20191007, 20191014) %>%
  monitor_subset(monitorIDs = "lon_.120.591_lat_38.714_arb2.1008")

Orleans <-
  monitor_load(20191007, 20191014) %>%
  monitor_subset(monitorIDs = "lon_.123.537_lat_41.304_arb2.1016")


targetTitle <- "Orleans Monitor and CANSAC Forecasts"
targetMonitor <- Orleans
targetLon <- targetMonitor$meta$longitude
targetLat <- targetMonitor$meta$latitude
timezone <- targetMonitor$meta$timezone


timeAxis <- seq(
  MazamaCoreUtils::parseDatetime(20191007, timezone = "UTC"),
  MazamaCoreUtils::parseDatetime(20191020, timezone = "UTC"),
  by = "hour"
)

# ----- Generic below here -----------------------------------------------------

plot(timeAxis, seq_along(timeAxis), las = 1,
     col = "transparent", ylim = c(0,500))

timeInfo <- timeInfo(timeAxis, targetLon, targetLat, timezone)
addShadedNight(timeInfo)

monitor_timeseriesPlot(targetMonitor, add = TRUE,
                       lwd = 2, col = "black")
title(targetTitle)

xlim <- c(targetLon - 0.5, targetLon + 0.5)
ylim <- c(targetLat - 0.5, targetLat + 0.5)


# bs_agg <- bluesky_aggregate(
#   model = "CANSAC-1.33km",
#   modelRun = 2019100900,
#   firstModelRun = 20191007,
#   lastModelRun = 20191013,
#   subDir = "forecast",
#   chunk = 1
# )


# ----- CANSAC_1.33km

CANSAC_1.33km_rasterBrick <- raster_aggregate(
  model = "CANSAC-4km",
  firstModelRun = 2019100700,
  lastModelRun = 2019101400,
  xlim = xlim,
  ylim = ylim
)


# CANSAC_1.33km_rasterBrick <- bluesky_load(
#   model = "CANSAC-1.33km",
#   modelRun = 2019101400,
#   modelType = "forecast",
#   xlim = xlim,
#   ylim = ylim
# )

CANSAC_1.33km_monitor <- raster_toMonitor(
  CANSAC_1.33km_rasterBrick,
  longitude = targetLon,
  latitude = targetLat,
  radius = 10
)

monitor_timeseriesPlot(
  CANSAC_1.33km_monitor,
  add = TRUE,
  type = "l",
  lwd = 1,
  col = adjustcolor(
    col = "salmon",
    alpha.f = 0.2,
    offset = c(0.1, 0.1, 0.1, 0) # more white"
  )
)

CANSAC_1.33km_Orleans <- monitor_collapse(
  CANSAC_1.33km_monitor,
  FUN = quantile,
  probs = 0.8,
  na.rm = TRUE
)

monitor_timeseriesPlot(
  CANSAC_1.33km_Orleans,
  add = TRUE,
  type = "b",
  lwd = 2,
  cex = 0.8,
  col = "salmon"
)


# ----- CANSAC_4km

# Now we can work as we normally would
CANSAC_4km <- bluesky_load(
  model = "CANSAC-4km",
  modelRun = 2019101400,
  subDir = "forecast",
  xlim = xlim,
  ylim = ylim
)

monitor_CANSAC_4km <- grid_createMonitor(
  CANSAC_4km,
  longitude = targetLon,
  latitude = targetLat,
  radius = 10000,
  monitorID = "Model data",
  FUN = quantile,
  probs = 0.80,
  na.rm = TRUE
)

monitor_timeseriesPlot(monitor_CANSAC_4km, add = TRUE,
                       type = "b", lwd = 1, cex=0.8, col = "dodgerblue")

# ----- PNW_4km

# Now we can work as we normally would
PNW_4km <- bluesky_load(
  model = "PNW-4km",
  modelRun = 2019101300,
  subDir = "forecast",
  xlim = xlim,
  ylim = ylim
)

# NOTE:  We get this when a model run isn't finished:
# NOTE:
# NOTE:  Error in grid_createMonitor(PNW_4km, longitude = targetLon, latitude = targetLat,  :
# NOTE:    Currently, only 3-D grids are supported.

# monitor_PNW_4km <- grid_createMonitor(
#   PNW_4km,
#   longitude = targetLon,
#   latitude = targetLat,
#   radius = 10000,
#   monitorID = "Model data",
#   FUN = quantile,
#   probs = 0.80,
#   na.rm = TRUE
# )
#
# monitor_timeseriesPlot(monitor_PNW_4km, add = TRUE,
#                        type = "b", lwd = 1, cex=0.8, col = "firebrick")


legend(
  "topleft",
  legend = c("CANSAC_1.33km", "CANSAC_4km"),
  col = c("salmon", "dodgerblue"),
  lwd = 1
)


# # Combine them and use AirMonitorPlots to plot them
# Yosemite_combined <-
#   PWFSLSmoke::monitor_combine(list(Yosemite_Village, Yosemite_fake))
#
# library(AirMonitorPlots)
#
# ggplot_pm25Timeseries(Yosemite_combined) +
#   geom_pm25Points(aes(color = monitorID)) +
#   stat_nowcast(aes(color = monitorID)) +
#   ggtitle("CANSAC-1.33km vs. Monitoring Data")
# }





