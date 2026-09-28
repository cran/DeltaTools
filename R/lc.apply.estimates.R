#' @noRd
lc.apply.estimates<-function(model, data, selected, x.field, mse, y.field, n.boots=100, forceEstimationAsymptoteToZero){
  data$X<-data[[x.field]]
  data$Y<-data[[y.field]]
  ldf<-data %>%
    dplyr::select(X, Y) %>%
    dplyr::arrange(X)
  # use bootstrapping method
  res<-nls.bootstrapping(model, boots=n.boots, ldf, form=selected$name, forceEstimationAsymptoteToZero)
  pdf<-res$preds %>%
    dplyr::mutate(
      estLearningRisk=.fitted
    ) %>%
    dplyr::rename(CaseOrder=X, Est=.fitted, LCL=lwr_CI, UCL=upr_CI) %>%
    dplyr::mutate(Est=round(Est*100, 4), LCL=round(LCL*100, 4), UCL=round(UCL*100, 4)) %>%
    dplyr::select(CaseOrder, Est, LCL, UCL, estLearningRisk) %>%
    dplyr::arrange(CaseOrder)
  return(list(preds=pdf, fig=res$fig, params=res$params))
}
