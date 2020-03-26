#' @title Load/Download a Raster Model
#'
#' @param model Model identifier(s).
#' @param run Date code as "YYYYMMDDHH" (integer or character).
#' @param xlim Optional longitude range.
#' @param ylim Optional latitude range.
#' @param local Absolute path of the local NetCDF (.nc) file
#' @param dirURL Model output web directory. Default is BlueSky standard output.
#' @param type Model type directory, i.e. 'forecast', 'combined', etc.
#' @param clean Logical specifying whether to remove the non-common format NetCDF.
#' @param verbose If \code{FALSE}, suppress status messages (if any).
#'
#' @description Load or download models and automatically convert NetCDF (.nc) format to
#' a spatio-temporal \code{Raster*} object.
#'
#' @details \code{model}, \code{run}, and \code{local} are all vectorised
#' parameters. As such, loading a model is executed using multiple CPU threads.
#'
#' @return A raster object, or a list of raster objects
#' @export
#'
#' @examples
#' \dontrun{
#' # Set Model download directory (Required)
#' setModelDataDir('~/Data/Bluesky')
#' # Load from server
#' model <- raster_load('PNW-1.33km', run = 20200303, xlim = c(-125, -115))
#' # Load from local NetCDF
#' model <- raster_load('~/Data/Bluesky/PNW-4km_2020030100.nc')
#' }
raster_load <- function(
  model = 'PNW-4km',
  run = NULL,
  xlim = NULL,
  ylim = NULL,
  local = NULL,
  dirURL = 'https://haze.airfire.org/bluesky-daily/output/standard',
  type = 'forecast',
  clean = FALSE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------
  MazamaCoreUtils::stopIfNull(dirURL)

  if ( is.null(run) ) { # Use todays date if run is null
    now <- lubridate::now(tzone = 'UTC')
    run <- paste0(strftime(now, '%Y%m%d', tz = 'UTC'), '00')
  } else {
    # Make sure we end up with YYYYmmddHH
    run <- as.character(run)
    length <- stringr::str_length(run)[1]
    if ( length == 8 ) { # if the length is 8 append 00 (model runs work YYYYmmddHH)
      run <- paste0(run, '00')
    }
  }
  # Verify YYYYmmddHH
  if ( !stringr::str_detect(run, '[0-9]{10}') ) {
    stop("'run' parameter must have 10 digits")
  }
  # Default to cleanup
  if ( !is.logical(clean) ) {
    warning('"clean" parameter must be logical boolean. Defaulting to FALSE')
    clean <- FALSE
  }
  # Default to verbose
  if ( !is.logical(verbose) ) {
    warning('"verbose" parameter must be logical boolean. Defaulting to TRUE')
    verbose <- TRUE
  }

  # ----- Load -----
  # Create a Parallel Socket Cluster
  cl <- parallel::makeCluster(future::availableCores() - 1)
  future::plan(strategy = future::cluster, workers = cl)
  data_dir <- getModelDataDir()

  model_list <- list()

  # NOTE: CHECK LOGIC
  # Create a list of models to load parallel
  if ( (length(model) >= 1) && is.null(local) && (length(run) <= 1) ) {
    for ( i in model ) {
      model_list[[i]] <- future::future({
        setModelDataDir(data_dir)
        .bluesky_load(i, run, xlim, ylim, local, dirURL, type, clean, verbose)
      })
    }
    load_check(model_list[[1]], paste0('Loading Model: ', paste(model, collapse = ', ')), verbose)
  } else if ( length(local) >= 1 ) { # Check if to load multiple local files
    for ( i in local ) {
      model_list[[i]] <- future::future({
        setModelDataDir(data_dir)
        .bluesky_load(model, run, xlim, ylim, i, dirURL, type, clean, verbose)
      })
    }
    load_check(model_list[[1]], paste0('Loading Local Files: ', paste(local, collapse = ', ')), verbose)
  } else if ( length(run) >= 1 ) {
    for ( i in run ) {
      model_list[[paste0(model,"_",i)]] <- future::future({
        setModelDataDir(data_dir)
        .bluesky_load(model, i, xlim, ylim, local, dirURL, type, clean, verbose)
      })
    }
    load_check(model_list[[1]], paste0('Loading Model Runs: ', paste(run, collapse = ', ')), verbose)
  } else {
    stop('Check parameters.')
  }

  models <- future::values(model_list)
  parallel::stopCluster(cl)

  return(models)

}

#' @title Internal Model Load Function
#'
#' @description This function encapsulates the process of downloading, and formatting
#' necessary for BlueSky model outputs. Additionally, it checks already existing
#' models and loads those if avaliable.
#'
#' @keywords Internal
#'
#' @param model A model. i.e 'PNW-1.33km' or 'CANSAC-4km'.
#' @param run The model run date. YYYYmmddHH format required.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylime A vector of coordinate latitude bounds.
#' @param local A path to a downloaded NetCDF model.
#' @param dirUrl the database URL.
#' @param clean Option to clean, or delete, the non-formatted model.
#' @param verbose Logical to display messages.
#'
#' @return A RasterBrick
.bluesky_load <- function(model, run, xlim, ylim, local, dirURL, type, clean, verbose) {

  if ( is.null(local) ) {

    setModelDataDir(getModelDataDir())
    bs_raw <- bluesky_download(model, run, dirURL, type, verbose)
    bs <- bluesky_toCommonFormat(bs_raw, clean = clean)

  } else {

    if ( file.exists(local) ) {
      v2_path <- stringr::str_replace(local, '.nc', '_V2.nc')
      if ( file.exists(v2_path) ) {
        bs <- v2_path
      } else {
        message('Converting NetCDF to common format. See help(bluesky_toCommonFormat()) for details.')
        bs <- bluesky_toCommonFormat(local, clean = FALSE)
      }
    } else {
      stop('Local NetCDF does not exist. Please check the local file location.')
    }

  }
  # Handle ylim and xlim
  # NOTE: raster bricks are not all loaded into memory, only their reference.
  if ( !is.null(xlim) || !is.null(ylim) ) {
    tmp_brick <- raster::brick(bs)
    if ( is.null(ylim) ) {
      ylim <- c(raster::ymin(tmp_brick), raster::ymax(tmp_brick))
    }
    if ( is.null(xlim) ) {
      xlim <- c(raster::xmin(tmp_brick), raster::xmax(tmp_brick))
    }
    if ( !any(findInterval(xlim, c(raster::xmin(tmp_brick), raster::xmax(tmp_brick)))) ) {
      stop('xlim out of model coordinate domain.')
    }
    if ( !any(findInterval(ylim, c(raster::ymin(tmp_brick), raster::ymax(tmp_brick)))) ) {
      stop('ylim out of model coordinate domain.')
    }
    cells <- raster::cellFromXY(tmp_brick, cbind(xlim, ylim))
    ext <- raster::extentFromCells(tmp_brick, cells)
    return(raster::crop(tmp_brick, ext))
  } else {
    return(raster::brick(bs))
  }

}


