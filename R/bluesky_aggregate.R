#' @importFrom stats median
#' @export
#' @title Aggregate BlueSky models by hour
#'
#' @param start A start date.
#' @param end A end date.
#' @param by Numeric vector. Represents hours to truncate models at.
#' @param model Model name (i.e. 'CANSAC-1.33km').
#' @param type Model type (i.e. 'forecast).
#' @param base_url The base url of the model output directory.
#' @param sub_dir The subdirectory of the output directory.
#' @param ... Optional parameters passed into `bluesky_load`.
#'
#' @return A Raster* brick
#'
bluesky_aggregate <- function(
  start,
  end,
  by = c(1,12),
  model = "CANSAC-4km",
  type = "forecast",
  base_url = "https://haze.airfire.org/bluesky-daily/output",
  sub_dir = "standard",
  version = "3.5",
  ...
  ) {

  # ----- Create time axis ------
  # Parse directory page for avaliable model runs
  html <- RCurl::getURL(paste( base_url,
                               sub_dir,
                               model, '/',
                               collapse = '/',
                               sep = '/' ))

  # NOTE: This selects evert other element in the dates
  #       The getURL function returns html with two listed directories for
  #       unknown reasons.
  aval_dates <- unlist(stringr::str_extract_all(html, '[0-9]{10}(?=/)'))[c(T,F)]
  parsed_dates <- MazamaCoreUtils::parseDatetime(aval_dates, timezone = "UTC")

  # Check if dates are valid
  if ( !(any(c(start, end) %in% stringr::str_extract(aval_dates, '[0-9]{1,8}'))) ) {
    stop('Start and/or End dates are not avaliable.')
  }

  # Calculate the median time difference of avaliable hours from directory
  # NOTE: Should probably find a better way to do this.
  t_diff <- median( (as.numeric(parsed_dates[c(F,T)]) -
                      as.numeric(parsed_dates[c(T,F)]))/(60**2) )

  # Warn user if the selected time difference is less than it should be
  if ( diff(by) + 1 < t_diff ) {
    warning(
      paste0(
        'Minimum continous model difference is ',
        t_diff,
        ' hours. Consider a larger "by" to avoid aggregated model gaps.'
      )
    )
  }

  # Parse start and end params. All timestamps are UTC.
  starttime <- MazamaCoreUtils::parseDatetime(start, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(end, timezone = "UTC")

  # Convert date sequence and convert to YMDH format
  run_dates <- strftime( seq(starttime, endtime, paste0(t_diff, ' hours')),
                         format = '%Y%m%d%H',
                         tz = 'UTC' )

  # Check desired run dates with avaliable dates
  valid_dates <- aval_dates[grepl(paste(run_dates, collapse = '|'), aval_dates)]

 # ----- Download model -----
  raster_list <- list()
  for ( run in valid_dates )  {
    # try({
    raster_list[[run]] <- bluesky_load( modelRun = run,
                                        model = model,
                                        baseUrl = base_url,
                                        dailyOutputDir = sub_dir,
                                        subDir = type,
                                        version = version,
                                        ... )
    # }, silent = TRUE)
  }

  # Convert raster stack list to brick
  raster_brick <- raster::brick(
    lapply(raster_list, function(r) r[[by[1]:by[2]]])
  )

  return(raster_brick)

  # === Debug ===
  if (FALSE) {
    start <- 20191015
    end <- 20191017
    by <- c(1,12)
    model <- 'PNW-4km'
    #'GFS-0.15deg-CanadaUSA-p25deg-68N'
    #"PNW1.33km-CMAQ"
    #'GFS-0.15deg-CanadaUSA-p25deg-68N'
    #"AK-12km"
    type <- "forecast"
    base_url <- "https://haze.airfire.org/bluesky-daily/output"
    sub_dir <- "standard"

    X1 <- bluesky_aggregate(20191115, 20191119, by = c(1,6), model = 'CANSAC-4km')
  }
}
