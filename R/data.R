#' Propensity Score Analysis and Inverse Probability of Treatment Weighting example data
#'
#' A synthetic patient population generated for use with the functions propensity score matching or
#' inverse probability of treatment weighting.
#'
#' @format ## `data.ps`
#' A data frame with 29,778 rows and 24 columns:
#' \describe{
#'   \item{Hypertension}{Hypertension indicator}
#'   \item{Mortality30D}{30-day mortality indicator}
#'   \item{Aspirin}{Aspirin use indicator}
#'   \item{Warfarin}{Warfarin use indicator}
#'   \item{CreatPreProc_mgdl}{Pre-procedure serum creatinine level in mg/dL}
#'   \item{FluoroMins}{Fluoroscopy time in minutes}
#'   \item{NSTEMIatPresent}{Non-ST-Elevation Myocardial Infarction at presentation indicator}
#'   \item{ProcDate}{Procedure date}
#'   \item{NumVesselsIndexPCI}{Number of blocked vessels at index percutaneous coronary intervention}
#'   \item{Patient}{Patient identifier}
#'   \item{Readmission}{Hospital readmission indicator}
#'   \item{PriorPCI}{History of percutaneous coronary intervention indicator}
#'   \item{Emergent}{Emergent case indicator}
#'   \item{TotalAdmitPCI}{Total admitted percutaneous coronary interventions}
#'   \item{ChrLungDis}{Chronic lung disease indicator}
#'   \item{Smoker}{Smoker indicator}
#'   \item{Female}{Female sex}
#'   \item{Dialysis}{Dialysis indicator}
#'   \item{Diabetes}{Diabetes indicator}
#'   \item{PeripheralArterialDis}{Peripheral arterial disease indicator}
#'   \item{RepeatProc}{Repeat procedure indicator}
#'   \item{DeviceB}{Device B indicator}
#'   \item{Age}{Patient age in years}
#'   \item{BMI}{Patient BMI in kg/m2}
#' }
"data.ps"

#' Parametric Learning Curve Analysis example data
#'
#' A synthetic patient population generated for learning effect and device signal detection using this function.
#'
#' @format ## `data.plc`
#' A data frame with 11,808 rows and 41 columns:
#' \describe{
#'   \item{Patient}{Patient identifier}
#'   \item{Operator}{Operator identifier}
#'   \item{Institution}{Institution identifier}
#'   \item{CaseOrder_All}{Case Order across all observations}
#'   \item{CaseOrder_Inst}{Case Order across that institution}
#'   \item{CaseOrder_Op}{Case Order across that operator}
#'   \item{ProcDate}{Procedure date}
#'   \item{CasePeriod}{Time period during which the case procedure occurred}
#'   \item{OutcomePeriod}{Time period during which the case outcome occurred}
#'   \item{CaseOrder_Inst_DevA}{Case Order across that institution and Device A}
#'   \item{CaseOrder_Inst_DevB}{Case Order across that institution and Device B}
#'   \item{CaseOrder_Op_DevA}{Case Order across that operator and Device A}
#'   \item{CaseOrder_Op_DevB}{Case Order across that oeprator and Device B}
#'   \item{Device}{Device placed during the procedure}
#'   \item{Outcome_Final}{Outcome variable}
#'   \item{Pt_F1}{Patient factor 1}
#'   \item{Pt_F2}{Patient factor 2}
#'   \item{Pt_F3}{Patient factor 3}
#'   \item{Pt_F4}{Patient factor 4}
#'   \item{Pt_F5}{Patient factor 5}
#'   \item{Pt_F6}{Patient factor 6}
#'   \item{Pt_F7}{Patient factor 7}
#'   \item{Pt_F8}{Patient factor 8}
#'   \item{Pt_F9}{Patient factor 9}
#'   \item{Pt_F10}{Patient factor 10}
#'   \item{Pt_F11}{Patient factor 11}
#'   \item{Pt_F12}{Patient factor 12}
#'   \item{Pt_F13}{Patient factor 13}
#'   \item{Pt_F14}{Patient factor 14}
#'   \item{Pt_F15}{Patient factor 15}
#'   \item{Pt_F16}{Patient factor 16}
#'   \item{Pt_F17}{Patient factor 17}
#'   \item{Pt_F18}{Patient factor 18}
#'   \item{Pt_F19}{Patient factor 19}
#'   \item{Pt_F20}{Patient factor 20}
#'   \item{Pt_F21}{Patient factor 21}
#'   \item{Op_F1}{Operator factor 1}
#'   \item{Op_F2}{Operator factor 2}
#'   \item{Inst_F1}{Institution factor 1}
#'   \item{Inst_F2}{Institution factor 2}
#'   \item{Inst_F3}{Institution factor 3}
#' }
"data.plc"
