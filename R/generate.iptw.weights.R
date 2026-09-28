#' @noRd
generate.iptw.weights<-function(data, allowIPTWWeighting, normalizeIPTWWeights, useGeneralCovariateFieldNMs,
                                covariateFieldNMs, iptwCovariateFieldNMs){
  if (allowIPTWWeighting==TRUE){
    if (useGeneralCovariateFieldNMs==T){
      covariates<-covariateFieldNMs
    } else {
      covariates<-iptwCovariateFieldNMs
    }
    ft<-models.generate.formular.string('exposure', covariates)
    model.t<-gbm::gbm(ft, distribution = "bernoulli", data=data, n.trees = 1000)
    data$pscores<-predict(model.t, type='response')
    data$weights <- ifelse(data$exposure==1, 1/data$pscores, 1/(1-data$pscores))
    # NORMALIZE WEIGHTS FOR GAM
    if (normalizeIPTWWeights==TRUE){
      data$weights<-data$weights/mean(data$weights)
    }
  } else {
    data$weights<-NA
  }
  return(data)
}
