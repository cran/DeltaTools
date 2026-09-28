#' @noRd
lc.quantification<-function(data, lcEstimationAlpha, learningRiskFieldNM, bootstraps, forceEstimationAsymptoteToZero){
  report<-list()
  estimatedForm<-lc.estimate.form(data=data, sequencer='operatorSeries', learningRiskFieldNM, alpha=lcEstimationAlpha, prefer.rse=TRUE)
  report$estimatedFormDetails<-estimatedForm
  if (is.na(estimatedForm$selected$name)==FALSE){
    # E) IF THERE'S A FORM SELECTED, ESTIMATE THE LEARNING RISK BASED ON THE FORM SELECTED
    edt<-estimatedForm$raw
    report$parametersEstimates<-edt
    report$formEstimated<-TRUE
    report$formEstimatedName<-estimatedForm$selected$name
    report$formParameterB<-estimatedForm$selected$b
    report$formParameterBLCI<-estimatedForm$selected$bll
    report$formParameterBUCI<-estimatedForm$selected$bul
    report$formParameterC<-estimatedForm$selected$c
    report$formParameterCLCI<-estimatedForm$selected$cll
    report$formParameterCUCI<-estimatedForm$selected$cul
    report$formParameterD<-estimatedForm$selected$d
    report$formParameterDLCI<-estimatedForm$selected$dll
    report$formParameterDUCI<-estimatedForm$selected$dul
    x<-estimatedForm$raw %>%
      dplyr::filter(form==tolower(estimatedForm$selected$name) & var=='a') %>%
      dplyr::select('Estimate')
    asymp<-x$Estimate
    data$asymp<-asymp
    report$asymp<-asymp
    estimated<-lc.apply.estimates(model=estimatedForm$model, data=data, selected=estimatedForm$selected,
                                  x.field='operatorSeries', mse=estimatedForm$ses, y.field='learningRiskOP',
                                  n.boots=bootstraps, forceEstimationAsymptoteToZero)
    report$estimated<-estimated$preds
    estimated$estLearningRiskWithAsymp<-estimated$preds$estLearningRisk+asymp
    estimated3<-estimated$preds %>%
      dplyr::arrange(CaseOrder)
    estimated2<-estimated3 %>%
      dplyr::distinct(CaseOrder, estLearningRisk)
    report$estimatedLearningCurve<-estimated2
    report$estimatedLearningCurvePlot<-estimated$fig
  } else {
    # No functional form is selected
    report$formEstimated<-FALSE
    report$message<-"No functional form selected..."
  }
  return(report)
}
