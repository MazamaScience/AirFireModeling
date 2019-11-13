#' @title ROC and AUC
#'
#' @param predicted Predicted logical.
#' @param observed Observed logical.
#' @param showPlot Show plot logical.
#'
#' @return a AUROC list
#' @export
#'
roc <- function(predicted, observed, showPlot = TRUE) {

  # Order observed based on predicted
  observed <- observed[order(predicted, decreasing = TRUE)]

  # ROC data frame
  roc <- data.frame( TPR = cumsum(observed) / sum(observed),
                     FPR = cumsum(!observed) / sum(!observed),
                     predicted )
  # Cryptic one-liner for AUC
  # By Miron Kursa https://mbq.me/blog/augh-roc/
  auc <- mean(rank(predicted)[observed] - 1:sum(observed)) / sum(!observed)

  # Return data list
  data <- list(ROC = roc, AUC = auc)

  # ggplot ROC
  if ( showPlot ) {
    ggplot2::ggplot(as.data.frame(data),
                    ggplot2::aes(x = data$ROC$FPR, y = data$ROC$TPR)) +
      ggplot2::geom_line() +
      ggplot2::ggtitle('ROC', subtitle = paste0('AUC: ', signif(data$AUC, 4))) +
      ggplot2::labs(x = 'False Positive Rate', y = 'True Positive Rate') +
      ggplot2::theme_light()
  }

  return(data)

}
