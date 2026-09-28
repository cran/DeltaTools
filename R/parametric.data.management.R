#' @noRd
parametric.data.management<-function(data, caseIDFieldNM, caseDateFieldNM, outcomeFieldNM, exposureFieldNM,
                                     exposureOfInterestNM, orderFieldNM, operatorFieldNM,
                                     exposureOfInterestOperatorCaseSeriesFieldNM, covariateFieldNMs,
                                     datasetIdentifier, periodIdentifier){
  # Data management for the parametric pipeline-returns simple metrics and a clean/formated dataset for further procesess.
  data <-as.data.frame(data)
  totalCases<-dim(data)[1]
  if (totalCases==0){
    # Stop if dataframe is empty
    output<-list(
      datasetIdentifier=datasetIdentifier,
      periodIdentifier=periodIdentifier,
      exposureOfInterestNM=exposureOfInterestNM,
      message="Data provided has no records...",
      totalCases=totalCases,
      totalOutcomeCount=0,
      exposureOfInterestCount=0,
      exposureOfInterestOutcomeCount=0,
      covariatesCount=length(covariateFieldNMs),
      data=data,
      passed=F
    )
  } else {
    # We have data, lets go
    data$caseID<-data[[caseIDFieldNM]]
    data$caseDate<-data[[caseDateFieldNM]]
    data$outcome <- data[[outcomeFieldNM]]
    data$exposureNM <- data[[exposureFieldNM]]
    data$orderID <- data[[orderFieldNM]]
    data$operatorNM <- data[[operatorFieldNM]]
    data$operatorSeries <- data[[exposureOfInterestOperatorCaseSeriesFieldNM]]
    keyFields<-c('caseID', 'caseDate', 'exposureNM', 'exposure', 'orderID', 'operatorNM', 'operatorSeries', 'outcome', covariateFieldNMs)
    data<-data %>%
      dplyr::arrange(orderID) %>%
      dplyr::mutate(
        operatorNM=as.factor(operatorNM),
        exposure=ifelse(exposureNM==exposureOfInterestNM, 1, 0),
        caseDate=as.Date(caseDate, format="%Y-%m-%d"),
        operatorSeries=as.integer(operatorSeries)
      ) %>%
      dplyr::select(all_of(keyFields)) %>%
      dplyr::arrange(orderID)
    # Generate counts
    data.a<-data %>%
      dplyr::filter(exposure==1)
    data.b<-data.a %>%
      dplyr::filter(outcome==1)
    data.c<-data %>%
      dplyr::filter(outcome==1)
    # Detect any covariates that are characters and convert them to factors
    for (i in covariateFieldNMs){
      if (is.character(data[[i]])==T){
        data[[i]]<-as.factor(data[[i]])
      }
    }
    output<-list(
      datasetIdentifier=datasetIdentifier,
      periodIdentifier=periodIdentifier,
      message=stringr::str_glue("Data provided has {totalCases} records..."),
      exposureOfInterestNM=exposureOfInterestNM,
      totalCases=totalCases,
      totalOutcomeCount=dim(data.c)[1],
      exposureOfInterestCount=dim(data.a)[1],
      exposureOfInterestOutcomeCount=dim(data.b)[1],
      covariatesCount=length(covariateFieldNMs),
      data=data,
      passed=T
    )
  }
  return(output)
}
