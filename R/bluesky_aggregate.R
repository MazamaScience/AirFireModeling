#' @importFrom stats median
#' @importFrom future `%plan%`
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
  startdate,
  enddate,
  by = c(1,12),
  model = "CANSAC-4km",
  type = "forecast",
  base_url = "https://haze.airfire.org/bluesky-daily/output",
  sub_dir = "standard",
  verbose = TRUE,
  clean = FALSE
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
  aval_dates <- unique(unlist(stringr::str_extract_all(html, '[0-9]{10}(?=/)')))
  # parsed_dates <- MazamaCoreUtils::parseDatetime(aval_dates, timezone = "UTC")
  parsed_dates <- lubridate::ymd_h(aval_dates)

  # Check if dates are valid
  if ( !(any(c(startdate, enddate) %in% stringr::str_extract(aval_dates, '[0-9]{1,8}'))) ) {
    stop('Start and/or End dates are not avaliable.')
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
  starttime <- lubridate::ymd_h(startdate, truncated = 2)#MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  endtime <- lubridate::ymd_h(enddate, truncated = 2) #MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")

  # Convert date sequence and convert to YMDH format
  run_dates <- strftime( seq(starttime, endtime, paste0(t_diff, ' hours')),
                         format = '%Y%m%d%H',
                         tz = 'UTC' )

  # Check desired run dates with avaliable dates
  # valid_dates <- aval_dates[grepl(paste(run_dates, collapse = '|'), aval_dates)]

 # ----- Download model -----
  # Create thread cluster
  # cl <- parallel::makeCluster(future::availableCores() - 1, timeout = 60)
  # future::plan(strategy = future::cluster, workers = cl)
  # raster_list <- list()
  # model_dir <- getModelDataDir()
  # for ( run in valid_dates )  {
  #   raster_list[[run]] <- future::future({
  #     setModelDataDir(model_dir)
  #     bluesky_load( modelRun = run,
  #                   model = model,
  #                   baseUrl = base_url,
  #                   dailyOutputDir = sub_dir,
  #                   subDir = type,
  #                   ... )
  #   })
  # }
  # # print loading bar
  # cat(paste0("Loading ", model, " "))
  # while(!future::resolved(raster_list[[1]])) {
  #   cat("=")
  #   Sys.sleep(2)
  # }
  # cat("\n")

  cl <- parallel::makeCluster(future::availableCores() - 1)
  future::plan(strategy = future::cluster, workers = cl)
  model_dir <- getModelDataDir()

  model_run <- list()

  for ( i in run_dates ) {
    model_run[[i]] <- future::future({
      setModelDataDir(model_dir)
      .load_brick(path = NULL, model_name = model, model_run = i, output_dir = sub_dir, sub_dir =  type, url = base_url, verbose = verbose, clean = clean, model_dir = model_dir)
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

  # === Debug ===
  if (FALSE) {
    startdate <- 20200301
    enddate <- 20200305
    by <- c(1,12)
    model <- 'PNW-4km'
    #'GFS-0.15deg-CanadaUSA-p25deg-68N'
    #"PNW1.33km-CMAQ"
    #'GFS-0.15deg-CanadaUSA-p25deg-68N'
    #"AK-12km"
    type <- "forecast"
    base_url <- "https://haze.airfire.org/bluesky-daily/output"
    sub_dir <- "standard"

    X1 <- bluesky_aggregate(20200301, 20200304, by = c(1,24), model = 'PNW-4km')
  }
}
