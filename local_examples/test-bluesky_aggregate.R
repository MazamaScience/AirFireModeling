# ----- Testing belusky_aggregate() --------------------------------------------

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("NaturalEarthAdm1")

library(AirFireModeling)

setModelDataDir("~/Data/Bluesky")

layout(matrix(seq(3)))

for ( chunk in 1:3 ) {
  
  timeAxis <- seq(
    MazamaCoreUtils::parseDatetime(20191009, timezone = "UTC"),
    MazamaCoreUtils::parseDatetime(20191016, timezone = "UTC"),
    by = "hour"
  )
  plot(timeAxis, seq_along(timeAxis), las = 1,
       col = "transparent", ylim = c(0,100))
  
  # Now do a smaller section
  agg_grid <- bluesky_aggregate(
    model = "PNW-4km",
    firstModelRun = 20191010,
    lastModelRun = 20191012,
    subDir = "combined",
    chunk = chunk
  )
  
  # Create a plot
  agg_grid %>%
    grid_createMonitor(
      longitude = -116.5,
      latitude = 47.2,
      radius = 10000,
      monitorID = "Model data",
      FUN = quantile,
      probs = 0.90,
      na.rm = TRUE
    ) %>%
    PWFSLSmoke::monitor_timeseriesPlot(shadedNight = TRUE, add = TRUE)
  title(paste0("Chunk ", chunk))
  
  # colors <- RColorBrewer::brewer.pal(7, "Dark2")
  # i <- 0
  # for ( modelRun in 20191010:20191012 ) {
  # 
  #   i <- i + 1
  # 
  #   # Now add the full model time axis
  #   bluesky_load(
  #     model = "PNW-4km",
  #     modelRun = modelRun,
  #     subDir = "combined"
  #   ) %>%
  #     grid_createMonitor(
  #       longitude = -116.5,
  #       latitude = 47.2,
  #       radius = 10000,
  #       monitorID = "Model data",
  #       FUN = quantile,
  #       probs = 0.90,
  #       na.rm = TRUE
  #     ) %>%
  #     PWFSLSmoke::monitor_timeseriesPlot(type='l', cex=0.5, pch=15, col=colors[i], add = TRUE)
  # 
  # }
  
}

layout(1)
