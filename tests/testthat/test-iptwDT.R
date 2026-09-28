# tests/testthat/test-iptwDT.R
library(testthat)
test_that("iptwDT works", {
  testthat::skip_on_cran()
  # Generate output
  wt <- iptwDT(data=data.ps,
               outcomes=c("Mortality30D", "Readmission", "RepeatProc"),
               exposure="DeviceB",
               covariates.factor=c("Aspirin", "ChrLungDis", "Diabetes", "Dialysis",
                                   "Emergent", "Female", "Hypertension", "NSTEMIatPresent",
                                   "PeripheralArterialDis", "PriorPCI", "Smoker", "Warfarin"),
               covariates.continuous=c("Age", "BMI", "CreatPreProc_mgdl", "FluoroMins",
                                       "NumVesselsIndexPCI", "TotalAdmitPCI"),
               identifier="Patient")
  # Evaluate class and structure
  expect_type(wt, "list")
  expect_named(wt,
               c("message", "outcome_models", "covariate_stats", "weighting_stats"),
               ignore.order=TRUE)
  # Evaluate Dimensions/Length
  expect_equal(nrow(wt$outcome_models),6)
  expect_equal(ncol(wt$outcome_models),14)
  # Evaluate Contents
  expect_equal(wt$message[[1]], "Treatment model completed")
  expect_equal(wt$message[[5]], "Weighting completed")
})
