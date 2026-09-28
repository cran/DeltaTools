#' @noRd
glm.get.artifacts.pscores<-function(pmodel, dependant){
    #generate pscore dataframe
    pscores<-predict(pmodel, type = "response")
    pscores_df<-as.data.frame(pscores)
    pscores_df$id<-row.names(pscores_df)
    exposures<-pmodel$model[dependant]
    res<-residuals(pmodel, type="deviance")
    pscores_df<-cbind(pscores_df, exposures, res)
    colnames(pscores_df)[3]<-'dependant'
    colnames(pscores_df)[4]<-'residuals'
    return(pscores_df)
}
