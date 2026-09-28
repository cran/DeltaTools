
# DeltaTools

[DeltaTools CRAN Page](https://cran.r-project.org/package=DeltaTools)

## Overview
The goal of DeltaTools is to provide functions to conduct parametric estimation 
of learning curves and device effects with modified versions of propensity score 
matching (PSM) and inverse probability of treatment weighting (IPTW).

## Installation
You can install the released version of DeltaTools from [CRAN](https://cran.r-project.org/package=DeltaTools) with:

``` r
install.packages("DeltaTools")
```

## Example

This is a basic example which shows you how to conduct a retrospective parametric learning curve analysis -
where a learning effect is not detected, but an unadjusted device signal is detected:

``` r
library(DeltaTools)

# basic example code
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
print(plc)
```

## Data Source Statement
The data included in DeltaTools was simulated using a specific data generating process for code testing purposes.

## Getting Help
If you encounter a bug, have a feature request, or need usage help, please email Amy.Perkins@vumc.org.

## License
`DeltaTools` is licensed under the [GPL-2](https://www.gnu.org) or [GPL-3](https://www.gnu.org) licenses.
