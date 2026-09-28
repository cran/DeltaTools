#' DeltaTools Function: PLCAnalysis
#'
#' @param data The data frame.
#' @param datasetIdentifier A character string with the dataset identifier.
#' @param caseIDFieldNM A character string with the case identifier.
#' @param caseDateFieldNM A character string with the case date field.
#' @param outcomeFieldNM A character string with the outcome.
#' @param orderFieldNM A character string with the case order field.
#' @param operatorFieldNM A character string with the operator field.
#' @param covariateFieldNMs A character vector of covariates to adjust for in the model.
#' @param exposureFieldNM A character string with the exposure.
#' @param exposureOfInterestNM A character string with the exposure value of interest.
#' @param exposureOfInterestOperatorCaseSeriesFieldNM A character string with the exposure of interest operator case series field.
#' @param allowIPTWWeighting Set to TRUE to allow Inverse Probability of Treatment Weighting.
#' @param normalizeIPTWWeights Set to TRUE to normalize IPTW weights.
#' @param useGeneralCovariateFieldNMs Set to TRUE to use general covariate field names for IPTW.
#' @param iptwCovariateFieldNMs A character vector of covariate field names for IPTW, used when useGeneralCovariateFieldNMs=TRUE.
#' @param allowOperatorClustering Set to TRUE to allow operator clustering.
#' @param allowGAMBasedAVS Set to TRUE to allow generalized additive model-based automatic variable selection.
#' @param learningUnadjustedSignalRequired Set to TRUE to require a learning unadjusted signal be detected before attempting to detect the presence of a learning effect.
#' @param leDetectionAlpha The alpha level for learning detection; the default value is 0.05.
#' @param lcEstimationAlpha The alpha level for learning curve estimation; the default value is 0.05.
#' @param bootstraps The default value for the number of bootstrapped samples is 0; may add other options in the future.
#' @param forceEstimationAsymptoteToZero Defaults to TRUE to force the estimation asymptote to zero; may add other options in the future.
#' @param deviceSignalEstimationAlpha The alpha level for device signal estimation; the default value is 0.05.
#'
#' @return A list containing relevant messages about detection of a learning effect and device signal, modeling summaries of the learning unadjusted signal, modeling summaries of the learning adjusted signal, learning detection summaries, and the GAM plot (if applicable).
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly toRGB
#' @importFrom pROC auc
#' @import lmtest
#' @import broom
#' @import dplyr
#' @import gbm
#' @import minpack.lm
#' @import stringr
#' @import mgcv
#' @import parameters
#' @export
#'
#' @examples
#' # Retrospective Parametric Learning Curve Analysis
#' # Learning effect not detected, unadjusted device signal detected
#' plc <- PLCAnalysis(data=data.plc,
#'                    datasetIdentifier="Dataset1",
#'                    caseIDFieldNM="Patient",
#'                    caseDateFieldNM="ProcDate",
#'                    outcomeFieldNM="Outcome_Final",
#'                    orderFieldNM="CaseOrder_All",
#'                    operatorFieldNM="Operator",
#'                    covariateFieldNMs=c("Pt_F1", "Pt_F2", "Pt_F3", "Pt_F4", "Pt_F5",
#'                                        "Pt_F6", "Pt_F7", "Pt_F8", "Pt_F9", "Pt_F10",
#'                                        "Pt_F11", "Pt_F12", "Pt_F13", "Pt_F14", "Pt_F15",
#'                                        "Pt_F16", "Pt_F17", "Pt_F18", "Pt_F19", "Pt_F20",
#'                                        "Pt_F21", "Op_F1", "Op_F2", "Inst_F1", "Inst_F2",
#'                                        "Inst_F3"),
#'                    exposureFieldNM="Device",
#'                    exposureOfInterestNM="B",
#'                    exposureOfInterestOperatorCaseSeriesFieldNM="CaseOrder_Op_DevB")
PLCAnalysis<-function(
    #=====================
    # Part 1: Data management
    #=====================
    data,
    datasetIdentifier="Dataset1",
    caseIDFieldNM,
    caseDateFieldNM,
    outcomeFieldNM,
    orderFieldNM,
    operatorFieldNM,
    covariateFieldNMs,
    #=====================
    # Part 2: Exposure
    #=====================
    exposureFieldNM,
    exposureOfInterestNM,
    exposureOfInterestOperatorCaseSeriesFieldNM,
    #=====================
    # Part 3: IPTW
    #=====================
    allowIPTWWeighting=FALSE,
    normalizeIPTWWeights=FALSE,
    useGeneralCovariateFieldNMs=TRUE,
    iptwCovariateFieldNMs=c(),
    #=====================
    # Part 4: GAM Modelling
    #=====================
    allowOperatorClustering=FALSE,
    allowGAMBasedAVS=FALSE,
    #=====================
    # Part 5: Learning effect detection
    #=====================
    learningUnadjustedSignalRequired=TRUE,
    leDetectionAlpha=0.05,
    #=====================
    # Part 6: Learning curve estimation
    #=====================
    lcEstimationAlpha=0.05,
    bootstraps=0,
    forceEstimationAsymptoteToZero=TRUE,
    #==========================================
    # Part 7: Intrinsic Device Signal Estimation
    #==========================================
    deviceSignalEstimationAlpha=0.05
){
    results<-parametric.lc.analysis(
      # Hyperparameters: Data management
      data,
      datasetIdentifier,
      periodIdentifier="period-001",
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
      allowIPTWWeighting,
      normalizeIPTWWeights,
      useGeneralCovariateFieldNMs,
      iptwCovariateFieldNMs,
      # Modelling
      allowOperatorClustering,
      allowGAMBasedAVS,
      # Learning effect detection
      learningUnadjustedSignalRequired,
      leDetectionAlpha,
      # Leaning curve estimation
      lcEstimationAlpha,
      bootstraps,
      forceEstimationAsymptoteToZero,
      # Intrinsic Device Signal Estimation
      deviceSignalEstimationAlpha
    )
    return(list(
      allowProspectiveAnalysis=FALSE,
      results=results
    ))
}
