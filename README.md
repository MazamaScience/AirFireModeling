# AirFireModeling R Package

```
Utilities for working with USFS AirFire BlueSky model output and PM2.5 
monitoring data available from AirNow, AIRSIS, WRCC and others.
```

## Background

The USFS Pacific Wildland Fire Sciences Lab [AirFire](http://www.airfire.org) 
team works to model wildland fire emissions and has created the BlueSky Modeling 
Framework. This system  integrates a wide collection of models along the smoke 
modeling chain (fire information, fuel loadings, consumption modeling, emissions 
modeling, time rate of emissions modeling, plume height estimations, and smoke 
trajectory and dispersion modeling). The resulting model output has been 
integrated into many different smoke prediction systems and scientific modeling 
efforts.

The **AirFireModeling** R package is being developed to help modelers and 
scientists better understand how the smoke predictions in their model output 
compare with smoke measurements made at monitoring sites.

The package includes functionality to make it easier to:

 * download and work with gridded BlueSky model output
 * download and work with non-gridded monitoring data
 * convert between gridded and non-gridded representations of the data
 * create maps and timeseries plots of gridded and non-gridded data
 * subset data based on spatial polygons (e.g. HUCs)
 * etc.

## Examples

Working example scripts that demonstrate the functionality of the package can be 
found in the `localExamples/` directory. This directory is part of the code base 
but not part of the R package.

----

This project is being funded by the USFS Pacific Wildland Fire Sciences 
Laboratory.


