#' @noRd
glm.get.artifacts<-function(pmodel){
    #take in a model object and output a well packaged model dataframe including ORs
    #coefficients
    coefficients<-pmodel$coefficients
    cofs_df<-as.data.frame(coefficients)
    cofs_df_unused<-subset(cofs_df, is.na(cofs_df$coefficients))
    cofs_df<-subset(cofs_df, !is.na(cofs_df$coefficients))
    cofs_df$odds_ratios<-exp(cofs_df$coefficients)
    cofs_df$variable<-row.names(cofs_df)
    # 95% CI for the coefficients
    estimates<-cofs_df
    a0<-parameters::model_parameters(pmodel, exponentiate = TRUE, digits=4, ci_digits=4, df_method="wald", ci_method="wald")
    a0$variable<-a0$Parameter
    a0$lower_ci_or<-a0$CI_low
    a0$upper_ci_or<-a0$CI_high
    a0<-a0[c('variable', 'lower_ci_or', 'upper_ci_or')]
    a1<-parameters::model_parameters(pmodel, exponentiate = FALSE, digits=4, ci_digits=4, df_method="wald", ci_method="wald")
    a1$variable<-a1$Parameter
    a1$lower_ci<-a1$CI_low
    a1$upper_ci<-a1$CI_high
    a1<-a1[c('variable', 'lower_ci', 'upper_ci')]
    confidence_intervals_df<-base::merge(a0, a1, by='variable')
    estimates<-merge(cofs_df, confidence_intervals_df, by='variable')
    #standard error
    standard_error<-summary(pmodel)$coefficients[, 2:4]
    se_df<-as.data.frame(standard_error)
    se_df$variable<-row.names(se_df)
    colnames(se_df)[1]<-'standard_error'
    colnames(se_df)[2]<-'test_stat'
    colnames(se_df)[3]<-'pvalue'
    estimates2<-merge(estimates, se_df, by='variable')
    estimates3<-estimates2 %>%
        dplyr::mutate_if(is.numeric, round, digits=4)
    return(estimates3)
}
