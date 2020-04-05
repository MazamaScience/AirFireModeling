library(AirFireModeling)

setModelDataDir('~/Data/Bluesky')
pnw_model_files <- bluesky_downloaded(pattern = 'PNW-4km_2019100[8-9][0-2]{2}_v2.nc', full = TRUE)

pnw_models <- raster_load(localPath = pnw_model_files)

maps <- raster_map(pnw_models)

n_sub <- raster_subsetByDistance(pnw_models, count = 20, longitude = -120, latitude = 45)
r_sub <- raster_subsetByDistance(pnw_models, radius = 10, longitude = -120, latitude = 45)

raster_map(n_sub, index = 20)
raster_map(r_sub, index = 20)

# monitors <- raster_toMonitor(pnw_models, longitude = -120, latitude = 45)
#
# ct <- raster_coordinateTrace(pnw_models, -120, 45)
#
# calculated <- raster_calculate(pnw_models, function(x) { x * 10 })
