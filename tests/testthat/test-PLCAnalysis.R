# tests/testthat/test-PLCAnalysis.R
library(testthat)
test_that("PLCAnalysis works", {
  # Generate output
  plc <- PLCAnalysis(data=data.plc,
                     datasetIdentifier="Dataset1",
                     caseIDFieldNM="Patient",
                     caseDateFieldNM="ProcDate",
                     outcomeFieldNM="Outcome_Final",
                     orderFieldNM="CaseOrder_All",
                     operatorFieldNM="Operator",
                     covariateFieldNMs=c("Pt_F1", "Pt_F2", "Pt_F3", "Pt_F4", "Pt_F5",
                                         "Pt_F6", "Pt_F7", "Pt_F8", "Pt_F9", "Pt_F10",
                                         "Pt_F11", "Pt_F12", "Pt_F13", "Pt_F14", "Pt_F15",
                                         "Pt_F16", "Pt_F17", "Pt_F18", "Pt_F19", "Pt_F20",
                                         "Pt_F21", "Op_F1", "Op_F2", "Inst_F1", "Inst_F2",
                                         "Inst_F3"),
                     exposureFieldNM="Device",
                     exposureOfInterestNM="B",
                     exposureOfInterestOperatorCaseSeriesFieldNM="CaseOrder_Op_DevB")
  # Evaluate class and structure
  expect_type(plc, "list")
  expect_named(plc, c("results", "allowProspectiveAnalysis"),
               ignore.order=TRUE)
  # Evaluate Dimensions/Length
  expect_equal(nrow(plc$results$learningUnadjustedSignal$model$model),11808)
  expect_equal(ncol(plc$results$learningUnadjustedSignal$model$model),28)
  # Evaluate Contents
  expect_false(plc$results$learningAdjustedSignal$hasSignal)
  # expect_equal(plc$results$comment, "Analysis stopped, operator learning not detected...")
})
