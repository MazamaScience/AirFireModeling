% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor_forecastPlot.R
\name{monitor_forecastPlot}
\alias{monitor_forecastPlot}
\title{Compare forecasts}
\usage{
monitor_forecastPlot(
  ws_monitor,
  modelName = NULL,
  modelMode = "forecast",
  baseUrl = "https://haze.airfire.org/bluesky-daily/output/standard",
  radius = 20,
  count = 9,
  prob = 0.5,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{ws_monitor}{ws_monitor object.}

\item{modelName}{Model identifier(s).}

\item{modelMode}{Subdirectory path containing BlueSky output, i.e. 'forcast'.}

\item{baseUrl}{Base URL for BlueSky output.}

\item{radius}{Distance (km) of radius from target location.}

\item{count}{Number of grid cells within radius to return.}

\item{prob}{Quantile probability used when  plotting model data.}

\item{verbose}{Logical to display messages.}

\item{...}{Additional arguments passed to
\code{PWFSLSmoke::monitor_timeseriesPlot()}.}
}
\description{
Plots monitor data and overlays models of the bluesky forecasts.

For each model, a timeseries is created by gathering \code{count} grid cells
within \code{radius} meters of the \code{ws_monitor}. The resulting set of
timeseries are then collapsed into a single timeseries by applying the
\code{quantile()} using the user specified probability \code{prob}.
}
\examples{
\donttest{
library(PWFSLSmoke)
initializeMazamaSpatialUtils()

library(AirFireModeling)
setModelDataDir('~/Data/Bluesky')

San_Pablo <-
  monitor_load(20191025, 20191029) \%>\%
  monitor_subset(monitorIDs = '060131004_01')

models <- bluesky_findModels(
  San_Pablo$meta$longitude,
  San_Pablo$meta$latitude
)

monitor_forecastPlot(
  San_Pablo,
  modelName = models
)
}
}
