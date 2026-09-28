# tests/testthat/test-propensityScoreAnalysis.R
library(testthat)
test_that("propensityScoreAnalysis works", {
  testthat::skip_on_cran()
  # Generate output
  ps <- propensityScoreAnalysis(data=data.ps,
                                outcomes=c("Mortality30D", "Readmission", "RepeatProc"),
                                exposure="DeviceB",
                                covariates.factor=c("Aspirin", "ChrLungDis", "Diabetes", "Dialysis",
                                                    "Emergent", "Female", "Hypertension", "NSTEMIatPresent",
                                                    "PeripheralArterialDis", "PriorPCI", "Smoker", "Warfarin"),
                                covariates.continuous=c("Age", "BMI", "CreatPreProc_mgdl", "FluoroMins",
                                                        "NumVesselsIndexPCI", "TotalAdmitPCI"),
                                identifier="Patient",
                                datefield="ProcDate",
                                match.date.range=TRUE,
                                m.replace=FALSE,
                                m.caliper=0.2)
  # Evaluate class and structure
  expect_type(ps, "list")
  expect_named(ps, c("message", "PS_Density", "outcomes_postmatch",
                     "balance_assessment", "matched_cohort"),
               ignore.order=TRUE)
  # Evaluate Dimensions/Length
  expect_equal(nrow(ps$outcomes_postmatch$outcomes_summary),3)
  expect_equal(ncol(ps$outcomes_postmatch$outcomes_summary),24)
  # Evaluate Contents
  expect_equal(ps$message, "Success")
  expect_equal(ps$balance_assessment$message, "Success")
})
