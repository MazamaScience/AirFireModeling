library(AirFireModeling)

setModelDataDir('~/Data/Bluesky')
pnw_model_files <- bluesky_downloaded(pattern = 'PNW-4km_.+_V2.nc', full = TRUE)

pnw_models <- raster_load(local = pnw_model_files)

maps <- raster_map(pnw_models)

n_sub <- raster_subset(pnw_models,n = 20, longitude = -120, latitude = 45)
r_sub <- raster_subset(pnw_models, radius = 10000, longitude = -120, latitude = 45)

monitors <- raster_toMonitor(pnw_models, longitude = -120, latitude = 45)
