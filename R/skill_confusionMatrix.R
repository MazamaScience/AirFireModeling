#' @title Confusion Matrix
#'
#' @param predicted Preicted logical.
#' @param observed Observed logical.
#'
#' @return a list of contingency statistics
#' @export
#' @examples
#' \dontrun{
#' predicted <- sample(c(T,F), 100, replace = TRUE)
#' observed <- sample(c(T,F), 100, replace = TRUE)
#' skill_confusionMatrix(predicted, observed)
#'}
skill_confusionMatrix <- function(predicted, observed) {

  # Checks
  if ( length(predicted) != length(observed) ) {
    stop(paste0("Length mismatch: predicted and observed must be same length."))
  }
  if ( !is.logical(predicted) | !is.logical(observed) ) {
    stop(paste0("Predicted and observed must be binary logic vectors."))
  }

  # Remove any elements where either predicted or observed has NA
  m <- matrix(c(predicted, observed), ncol = 2)
  badRows <- apply(m, 1, function(x) { any(is.na(x)) })
  predicted <- m[!badRows,1]
  observed <- m[!badRows, 2]

  # True Positive
  tp <- sum(predicted & observed)
  # False Positive
  fp <- sum(predicted & !observed)
  # True Negative
  tn <- sum(!predicted & !observed)
  # False Negatve
  fn <- sum(!predicted & observed)

  # Confusion Matrix
  #
  #                 Observed Class
  # Predicted Class  Negative Positive
  #        Negative     tn       fp
  #        Positive     fn       tp
  #
  con_matrix <- matrix(
    c(sum(tn), sum(fp), sum(fn), sum(tp)),
    nrow = 2,
    dimnames = list('Predicted Class'= c('Negative', 'Positive'),
                    'Observed Class'= c('Negative', 'Positive'))
  )

  # Rates
  tp_rate <- tp / (tp + fn)
  fp_rate <- fp / (fp + tn)
  tn_rate <- tn / (tn + fp)
  fn_rate <- fn / (fn + tp)

  # Accuracy
  acc <- (tp + tn) / (tp + tn + fp + fn)
  # Error
  err <- 1 - acc
  # Precision or positive predictive value
  pp_value <- prec <- tp / (tp + fp)
  # Negative predictive value
  np_value <- tn / (tn + fn)
  # False discovery rate
  fd_rate <- fp / (fp + tp)
  # False omission rate
  fo_rate <- fn / (fn + tn)
  # Prevalence
  prev <- sum(observed) / (tp + tn + fp + fn)
  # F1 Score
  f1_score <- 2 * (prec * tp_rate) / (prec + tp_rate)
  # Mattews Correlation Coefficent
  # https://en.wikipedia.org/wiki/Matthews_correlation_coefficient
  #mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fn) * (tn + fp))
  # Cohen's Kappa
  ex_acc <-
    ( (sum(predicted) * sum(observed) / (tp + tn + fp + fn)) +
        (sum(!predicted) * sum(!observed) / (tp + tn + fp + fn)) ) / (tp + tn + fp + fn)
  kappa <- (acc - ex_acc) / (1 - ex_acc)

  # Return data list
  data <- list( table = as.table(con_matrix),
                TPR = tp_rate,
                FPR = fp_rate,
                TNR = tn_rate,
                FNR = fn_rate,
                ACC = acc,
                PPV = prec,
                NPV = np_value,
                FDR = fd_rate,
                FOR = fo_rate,
                PREV = prev,
                F1 = f1_score,
                #MCC = mcc,
                KAPPA = kappa )

  return(data)

  # ---- Debug ----
  if ( FALSE ) {
    predicted <- sample(c(T,F), 100, replace = TRUE)
    observed <- sample(c(T,F), 100, replace = TRUE)

    skill_confusionMatrix(predicted, observed)
  }

}
