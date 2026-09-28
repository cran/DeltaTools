#' @noRd
parametric.lc.analysis<-function(
  # Hyperparameters: Data management
  data,
  datasetIdentifier,
  periodIdentifier,
  caseIDFieldNM,
  caseDateFieldNM,
  outcomeFieldNM,
  orderFieldNM,
  operatorFieldNM,
  covariateFieldNMs,
  # Hyperparameters: exposures
  exposureFieldNM,
  exposureOfInterestNM,
  exposureOfInterestOperatorCaseSeriesFieldNM,
  # Hyperparameters: iptw
  allowIPTWWeighting=FALSE,
  normalizeIPTWWeights=FALSE,
  useGeneralCovariateFieldNMs=TRUE,
  iptwCovariateFieldNMs=c(),
  # Modelling
  allowOperatorClustering=FALSE,
  allowGAMBasedAVS=FALSE,
  # Learning effect detection
  learningUnadjustedSignalRequired=TRUE,
  leDetectionAlpha=0.05,
  # Leaning curve estimation
  lcEstimationAlpha=0.05,
  bootstraps=1,
  forceEstimationAsymptoteToZero=TRUE,
  # Intrinsic Device Signal Estimation
  deviceSignalEstimationAlpha=0.05
){
  # Perform a parametric learning curve analysis
  # Manage the data
  dm<-parametric.data.management(data, caseIDFieldNM, caseDateFieldNM, outcomeFieldNM, exposureFieldNM,
                                 exposureOfInterestNM, orderFieldNM, operatorFieldNM,
                                 exposureOfInterestOperatorCaseSeriesFieldNM, covariateFieldNMs,
                                 datasetIdentifier, periodIdentifier)
  output<-list(inputData=dm)
  ##Only proceed if data management passes
  if (dm$passed==TRUE){
    # Run IPTW if the user allows treatment weighting, weights will be added to the dataset or NA if not
    data.iptw<-generate.iptw.weights(dm$data, allowIPTWWeighting, normalizeIPTWWeights, useGeneralCovariateFieldNMs,
                                     covariateFieldNMs, iptwCovariateFieldNMs)
    # If operator clustering is allowed, create a string to be added to the GAM model formula
    clus<-c()
    if (allowOperatorClustering==T){
      clus<-'s(operatorNM, bs="re")'
    }
    # Let us check if there is a raw signal (no learning adjustment)
    liuFeatures<-c("exposure", covariateFieldNMs, clus)
    data.iptw<- data.iptw %>%
      dplyr::arrange(exposure, operatorNM, operatorSeries) %>%
      as.data.frame()

    signal.liu<-signal.detection.gam(target="outcome", features=liuFeatures, data=data.iptw, method="REML",
                                     allowAVS=allowGAMBasedAVS, allowWeights=allowIPTWWeighting, alphaLevel=0.05)
    output$learningUnadjustedSignal=signal.liu

    if (learningUnadjustedSignalRequired==TRUE & signal.liu$hasSignal==FALSE){
      # Let us check if a raw signal exists before we attempt to detect learning effect presence
      output$learningUnadjustedSignalRequired<-TRUE
      output$signal.liu$hasSignal<-FALSE
      output$comment<-"Analysis stopped, learning unadjusted signal not detected..."
      return(output)
    } else {
      # Proceed
      # Learning unadjusted model
      liu<-signal.liu$model

      # Learning adjusted model
      # For cases that don't have the exposure of interest, let us set their operatorSeries value to equal to the max operatorSeries of the exposure of interest
      eoi<-data.iptw %>%
        dplyr::filter(exposure==1) %>%
        dplyr::summarize(
          maxOS=max(operatorSeries)
        )
      data.iptw$operatorSeries<-ifelse(data.iptw$exposure==0, eoi$maxOS, data.iptw$operatorSeries)
      liaFeatures<-c("exposure", covariateFieldNMs, clus, "s(operatorSeries, k=50, bs='ad')")
      signal.lia<-signal.detection.gam(target="outcome", features=liaFeatures, data=data.iptw, method="REML",
                                       allowAVS=allowGAMBasedAVS, allowWeights=allowIPTWWeighting,
                                       alphaLevel=deviceSignalEstimationAlpha)
      output$learningAdjustedSignal=signal.lia
      lia<-signal.lia$model
      output$GAMplot<-mgcv::plot.gam(lia, ylab="Predicted probability of adverse event")

      # DETECT LEARNING USING THE LR TEST
      det<-learning.detection(liu, lia, leDetectionAlpha)
      output$learningDetection<-det
      if (det$level!="operator"){
        # No operator learning detected, stop
        output$comment<-"Analysis stopped, operator learning not detected..."
        return(output)
      } else {
        # Proceed now that operator learning effect has been detected
        # ESTIMATE LEARNING RISK
        doi0<-data.iptw %>%
          dplyr::filter(exposure==1) %>%
          dplyr::mutate(operatorSeries2=operatorSeries)
        doi0$overallRisk<-predict(lia, type = 'response', newdata=doi0)
        doi0$operatorSeries<-eoi$maxOS
        doi0$patientDeviceRisk<-predict(lia, type = 'response', newdata=doi0)
        doi0<-doi0 %>%
          dplyr::mutate(
            operatorSeries=operatorSeries2,
            learningRiskOP=overallRisk-patientDeviceRisk
          ) %>%
          dplyr::select(caseID, operatorNM, operatorSeries, overallRisk, patientDeviceRisk, learningRiskOP)
        # ESTIMATE LEARNING FORM
        lc_est<-lc.quantification(data=doi0, lcEstimationAlpha, learningRiskFieldNM='learningRiskOP',
                                  bootstraps, forceEstimationAsymptoteToZero)
        output$lcQuantification<-lc_est
        output$lcQuantification$dataUsed<-doi0
        return(output)
      }
    }
  }
  else {
    return(output)
  }
}
