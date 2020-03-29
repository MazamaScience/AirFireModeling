#' @title ROC and AUC
#'
#' @param predicted Predicted logical.
#' @param observed Observed logical.
#' @param p_thresh low and high values used to generate test thresholds for
#' classifying \code{predicted} data.
#' @param o_thresh value used to classify \code{observed} data.
#' @param n number of test thresholds in ROC curve.
#'
#' @return a AUROC list
#' @export
#' @examples
#' \dontrun{
#' ca <- airnow_loadAnnual(2017) %>%
#'   monitor_subset(tlim = c(20171001,20171101), stateCodes = 'CA')
#' Vallejo <- monitor_subset(ca, monitorIDs = '060950004_01')
#' Napa <- monitor_subset(ca, monitorIDs = '060550003_01')
#' skill_roc(predicted = Vallejo, observed = Napa, n = 10, o_thresh = 10, p_thresh = c(1,100))
#'}
skill_roc <- function(
  predicted,
  observed,
  p_thresh = c(1,100),
  o_thresh = 55.5,
  n = 100
) {

  # Extract data from ws_monitor objects
  if ( 'ws_monitor' %in% class(predicted) ) {
    if ( ncol(predicted$data) > 2 ) {
      stop(paste0('predicted must represent a single monitoring location'))
    } else {
      predicted <- predicted$data[[2]]
    }
  }

  # Extract data from ws_monitor objects
  if ( 'ws_monitor' %in% class(observed) ) {
    if ( ncol(observed$data) > 2 ) {
      stop(paste0('observed must represent a single monitoring location'))
    } else {
      observed <- observed$data[[2]]
    }
  }

  # Remove any elements where either predicted or observed has NA
  m <- matrix(c(predicted, observed), ncol = 2)
  badRows <- apply(m, 1, function(x) { any(is.na(x)) })
  predicted <- m[!badRows, 1]
  observed <- m[!badRows, 2]

  # Create threshold ranges to test
  thresh_range <- seq(p_thresh[1], p_thresh[2], length.out = n)

  # Create confusion matrix and other contingency stats
  # Use a range of thresholds from (p)redicted (thresh)olds to iterate through
  data <- do.call(rbind,
                  lapply( X = thresh_range,
                          FUN = function(x) {
                            skill_confusionMatrix( predicted >= x,
                                             observed >= o_thresh )
                          } ))

  # AUC
  auroc <- function(tpr, fpr) {
    auc <- abs(sum(1/2 * diff(tpr) * diff(fpr)) +
                 sum(diff(fpr) * tpr[-length(tpr)]))
    return(auc)
  }

  tpr <- unlist(data[,2]) # Extract TP Rate
  fpr <- unlist(data[,3]) # Extract FP Rate
  auc <- auroc(tpr, fpr)
  # Get Heidke Skill
  kappa <- skill_confusionMatrix(predicted >= o_thresh, observed >= o_thresh)$KAPPA

  plt <- ggplot2::ggplot(mapping = ggplot2::aes(x = fpr, y = tpr)) +
    ggplot2::geom_step() +
    ggplot2::theme_light() +
    ggplot2::labs( title = 'ROC',
                   subtitle = paste0('Threshold:', o_thresh, '\t',
                                     'AUC:', signif(auc, 3), '\t',
                                     'Heidke Skill:', signif(kappa, 3)),
                   x = 'False Positive Rate',
                   y = 'True Positive Rate' )

  # Return List
  roc_data <- list( Threshold = thresh_range,
                    TPR = tpr,
                    FPR = fpr,
                    Kappa = kappa,
                    AUC = auc,
                    plot = plt )

  return(roc_data)


  # ---- Debug ----
  if ( FALSE ) {

    ca <- airnow_loadAnnual(2017) %>%
      monitor_subset(tlim = c(20171001,20171101), stateCodes = 'CA')
    Vallejo <- monitor_subset(ca, monitorIDs = '060950004_01')
    Napa <- monitor_subset(ca, monitorIDs = '060550003_01')
    skill_roc(predicted = Vallejo, observed = Napa, n = 10, o_thresh = 10, p_thresh = c(1,100))
  }

}
