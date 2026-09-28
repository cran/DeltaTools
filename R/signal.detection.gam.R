#' @noRd
signal.detection.gam<-function(target, features, data, method="REML", allowAVS=F, allowWeights=F, alphaLevel=0.05){
  fm<-models.generate.formular.string(target, features)
  smodel<-mgcv::bam(fm, data=data, method=method, family = binomial('logit'), select=allowAVS)
  if (allowWeights==T){
    smodel<-mgcv::bam(fm, data=data, method=method, family = binomial('logit'), select=allowAVS, weights=data$weights)
  }
  data$estimated_baseline_probabilities<-predict(smodel, type='response')
  data<-data %>%
    dplyr::select(caseID, outcome, estimated_baseline_probabilities)
  mps<-parameters::model_parameters(smodel, exponentiate = TRUE, pretty_names = TRUE, summary=TRUE)
  sart<-as.data.frame(mps)
  sres<-subset(sart, Parameter=='exposure')
  od<-round(as.numeric(sres['Coefficient']), 4)
  odl<-round(as.numeric(sres['CI_low']), 4)
  odu<-round(as.numeric(sres['CI_high']), 4)
  pv<-round(as.numeric(sres['p']), 4)
  yes<-ifelse(odl>1 & pv<=alphaLevel, TRUE, FALSE)
  auc<-suppressMessages(pROC::auc(data$outcome, data$estimated_baseline_probabilities))
  return(list(
    form=fm,
    model=smodel,
    model.type='gam',
    estimates=sart,
    hasSignal=yes,
    signal=od,
    signalLower=odl,
    signalUpper=odu,
    pvalue=pv,
    data=data,
    AIC=AIC(smodel),
    AUC=as.numeric(auc),
    alphaLevel=alphaLevel
  ))
}
