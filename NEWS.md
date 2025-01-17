# AirFireModeling 0.3.3

* Revamped time-layer names in Raster* objects.
* Added new function `raster_getTime()` to help end-users determine Raster* object times.
* Updated examples to use Oregon Labor Day Fires from 2020.

# AirFireModeling 0.3.2

* 'bluesky_download()' is more tolerant of Bluesky output version matching, now 
only matching `major` but not `.minor.patch` levels.

# AirFireModeling 0.3.1

* Fixed error when using `raster_spaghettiPlot()` on a single `rasterBrick` object.
* Improved `raster_leaflet()` visual consistency by adding rasters using
"nearest neighbor" interpolation rather than default "bilnear",

# AirFireModeling 0.3.0

More regularization througout the code base:

* Changed `model` to `modelName`.
* Changed `modelType` to `modelMode`.
* New "Lazy Evaluation" article.

# AirFireModeling 0.2.19

* Improved/consistent examples for all functions.

# AirFireModeling 0.2.18

* Fixed bug in `raster_createLayerNameTimes()`.

# AirFireModeling 0.2.17

* Added `cmaq_toCommonFormat()` to process CMAQ model runs.

# AirFireModeling 0.2.16

* Completely refactored `raster_spaghettiPlot()` now returns a ggplot object.
* New `bluesky_latestModelRun()` function returns available model runs.

# AirFireModeling 0.2.15

* Removed need to specify `modelDataDir` when using `bluesky_load()` with the
`localPath` argument. Converted data files will be saved in the same directory
as the file specified with `localPath`.
* Added `format` parameter to `raster_createTimeStrings()`.
* Removed `raster_createTimeStamps()` as this functionality is no longer needed.
* Added "Smoke by County" vignette.

# AirFireModeling 0.2.14

* Renamed `raster_map()` to `raster_ggmap()` to make it clear that it returns
a *ggplot2* plot object.
* Added `monitor_forecastPlot()`.
* Added "Aggregation by Polygon" vignette.

# AirFireModeling 0.2.13

* Added support for 'aqi' colors to `raster_leaflet()`.
* Finished "Introduction to AirFireModeling" vignette.
* Added support for `localPath` in `bluesky_load()` and `raster_load()`.

# AirFireModeling 0.2.12

* Restored improved `monitor_toRaster()`.

# AirFireModeling 0.2.11

* Improvements to `raster_spaghettiPlot()`.

# AirFireModeling 0.2.10

* Improvements to `raster_spaghettiPlot()`.
* Removed `skill_roc()` and `skill_confusionMatrix()`.
* Removed `raster_correlation()`.
* Removed `monitor_forecast()`, `raster_coordnanteTrace()` and
`raster_loadNearestMonitor()`.

# AirFireModeling 0.2.9

* New `raster_subsetByPolygon()` function.
* Removed use of "promises" (*parallel* and *future* packages) everywhere 
because it interferes with *raster* package delayed evaluation.
* Improvements to `raster_aggregate()`.
* Improvements to `raster_toMonitor()`.

# AirFireModeling 0.2.8

* Improvements to `raster_map()`.
* Improvements to `raster_leaflet()`.
* Improvements to `raster_facet()`.
* Removed `raster_calculate()`.
* Renamed `raster_subset()` to `raster_subsetByDistance()`. It now uses
explicit arguments rather than `...`.
* Added `col_state` and `col_county` arguments to all mapping functions to allow
user specification of state and county border colors.
* Using model names as titles in `raster_map()`.
* New `raster_loadNearestMonitor()` function.

# AirFireModeling 0.2.7

* Internal refactoring and cleanup.
* Removed `localPath` support from `raster_load()`. Local files not found in the
`modelDataDir` should be loaded with `bluesky_load()`.
* Removed use of "promises" (*parallel* and *future* packages) in the 
`raster_load()` function because it interferes with *raster* package delayed
evaluation.

# AirFireModeling 0.2.6

* Internal documentation cleanup.
* Using consistent capitalization whenever "BlueSky" or "v2" is used.
* Renamed various variables to use "well described nouns".
* Renamed various `bluesky_` functions to use "well described verbs".
* Using a more pedantic coding style in some functions.

# AirFireModeling 0.2.5

* Updated to multithreaded capability 
* New names - `bluesky_` is replaced with `raster_` functions with the execption of those specific to the BlueSky model.
* `raster_` functions vectorised to support lists of parameters 
* Updated the parameters for consistency
* Added `bluesky_downloads()`

# AirFireModeling 0.2.1

* Minor cleanup to pass R CMD check.

# AirFireModeling 0.2.0

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
* Moved other non-rewritten and useful functions to `utils/`. 

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
