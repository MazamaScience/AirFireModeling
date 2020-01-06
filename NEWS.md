# AirFireModeling 0.2

* Migrated to `raster` base 
* Moved deprecated base to `old_R/`.
* Added correlation rasters `raster_correlation()` 
* Added `raster_spaghetti()`
* Added `raster_subset()` and various subset capabilites
* Added `raster_toMonitor()` and `monitor_toRaster()` conversion
* Added `raster_map()`
* Added `monitor_forecast()` for monitor forecast from rasters 
* Added `bluesky_*()` functions for downloading and assimilating NetCDF's as rasters 
* Added `skil_*()` functions
* Added `bluesky_aggregate()` to aggregate multi-day-model runs by hours
* Moved oher non-rewritten and useful functions to `utils/`. 

# AirFireModeling 0.1.6

* New `bluesky_modelInfo` tibble with model metadata.
* Renamed `modelBoundingBox()` to `grid_boundingBox()`.

# AirFireModeling 0.1.5

* Cleanup of `grid_timeseriesPlot()`
* Cleanup of `grid_spaghettiPlot()`
* Cleanup of `grid_correlationMap()`
* Cleanup of `monitor_forecastPlot()`

# AirFireModeling 0.1.4

* Added `grid_spaghettiPlot()`
* Added `grid_timeseriesPlot()`
* Added `modelBoundingBox()`
* Include ggplot2 option in `monitor_forecastPlot()`
* Added `grid_correlationMap()`

# AirFireModeling 0.1.3

* New `monitor_forecastPlot()` function plots monitor data and model forecasts
at the monitor location.

# AirFireModeling 0.1.2

* Refactored `bluesky_load()` to handle `xlim` and `ylim` arguments.

# AirFireModeling 0.1.1

* Refactored `bluesky_load()` to handle `timesteps` argument.
* Added `bluesky_aggregate()`.

# AirFireModeling 0.1.0

* Initial `bluesky_~()` and `grid_~()` functions.
