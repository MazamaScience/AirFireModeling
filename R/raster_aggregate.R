raster_aggregate <- function(
  model = 'PNW-4km',
  startdate,
  enddate,
  by = c(1,12), # maybe try "12 hours"
  xlim = NULL,
  ylim = NULL,
  local = NULL,
  dirURL = 'https://haze.airfire.org/bluesky-daily/output/standard',
  type = 'forecast',
  clean = FALSE,
  verbose = TRUE
) {

  url <- paste(dirURL, model, collapse = '/', sep = '/')
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
    warning(
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

  cl <- parallel::makeCluster(future::availableCores() - 1)
  future::plan(strategy = future::cluster, workers = cl)
  model_dir <- getModelDataDir()

  model_run <- list()

  for ( i in run_dates ) {
    model_run[[i]] <- future::future({
      setModelDataDir(model_dir)
      .bluesky_load(model, i, xlim, ylim, local, dirURL, type, clean, verbose)
    })
  }

  cat("Loading ")
  while(!future::resolved(model_run[[1]])) {
    cat(".")
    Sys.sleep(2)
  }
  cat("\n")

  models <- future::values(model_run)
  parallel::stopCluster(cl)

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


