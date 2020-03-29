#' @title Aggregate Raster Models
#'
#' @param model Model identifier(s).
#' @param startdate Desired start date (integer or character in ymd[hms] format or POSIXct).
#' @param enddate Desired end date (integer or character in ymd[hms] format or POSIXct).
#' @param by Numeric vector. Represents hours to truncate models at.
#' @param xlim Optional longitude range.
#' @param ylim Optional latitude range.
#' @param localPath Absolute or relative paths of the local NetCDF (.nc) files.
#' @param baseUrl Model output web directory. Default is BlueSky standard output.
#' @param modelType Model type directory, i.e. 'forecast', 'combined', etc.
#' @param clean Logical specifying whether to remove the non-common format NetCDF.
#' @param verbose If \code{FALSE}, suppress status messages (if any).
#'
#' @return A raster object.
#' @export
#'
raster_aggregate <- function(
  model = 'PNW-4km',
  startdate,
  enddate,
  by = c(1,12), # maybe try "12 hours"?
  xlim = NULL,
  ylim = NULL,
  localPath = NULL,
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  modelType = 'forecast',
  clean = FALSE,
  verbose = TRUE
) {

  url <- paste(baseUrl, model, collapse = '/', sep = '/')
  aval_dates <- unique(unlist(stringr::str_extract_all(readLines(url), '[0-9]{10}(?=/)')))
  parsed_dates <- lubridate::ymd_h(aval_dates)
  # Check if dates are valid
  if ( !(any(c(startdate, enddate) %in% stringr::str_extract(aval_dates, '[0-9]{1,8}'))) ) {
    stop('startdate and/or enddate are not avaliable.')
  }

  # Calculate the median time difference of available hours from directory
  t_diff <- diff(parsed_dates)[parsed_dates == lubridate::ymd_hms(enddate, truncated = 6)]
  # Warn user if the selected time difference is less than it should be
  if ( diff(by) + 1 < as.numeric(t_diff) ) {
    message(
      paste0(
        'Minimum continous model difference is ',
        t_diff,
        ' hours. Consider a larger "by" to avoid aggregated model gaps.'
      )
    )
  }

  # if t_diff is 24 hour, past 00 if t diff is 12 hour paste 00 12 etc

  # Parse start and end params. All timestamps are UTC.
  parsed_start <- lubridate::ymd_h(startdate, truncated = 2)
  parsed_end <- lubridate::ymd_h(enddate, truncated = 2)
  # Convert date sequence and convert to YMDH format
  run_dates <- strftime( seq(parsed_start, parsed_end, paste0(t_diff, ' hours')),
                         format = '%Y%m%d%H',
                         tz = 'UTC' )

  model_dir <- getModelDataDir()
  setModelDataDir(model_dir)
  models <- raster_load( model = model,
                         modelRun = run_dates,
                         xlim = xlim,
                         ylim = ylim,
                         localPath = localPath,
                         baseUrl = baseUrl,
                         modelType = modelType,
                         verbose = verbose )

  # Convert raster stack list to brick
  model_brick <- raster::brick(
    lapply(
      X = models,
      FUN = function(r) {
        r[[by[1]:by[2]]]
      }
    )
  )

  return(model_brick)

}


