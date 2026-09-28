#' @noRd
glm.flag.unused.features<-function(model.data, features){
    #flag unused features
    features.used<-as.vector(model.data$variable)
    features<-as.vector(features)
    features.unused<-vectors.compare(features, features.used)
    a<-data.frame(variable=as.character(),
                  coefficients=as.numeric(),
                  odds_ratios=as.numeric(),
                  lower_ci=as.numeric(),
                  upper_ci=as.numeric(),
                  lower_ci_or=as.numeric(),
                  upper_ci_or=as.numeric(),
                  standard_error=as.numeric(),
                  test_stat=as.numeric(),
                  pvalue=as.numeric(),
                  comment=as.character())
    if (length(features.unused)>0){
        for (i in features.unused){
            b<-data.frame(variable=i,
                          coefficients=NA,
                          odds_ratios=NA,
                          lower_ci=NA,
                          upper_ci=NA,
                          lower_ci_or=NA,
                          upper_ci_or=NA,
                          standard_error=NA,
                          test_stat=NA,
                          pvalue=NA,
                          comment='Not used, singularity possible'
            )
            a<-rbind(a, b)
        }
        return(list(data=rbind(model.data, a), features.unused=features.unused, features.used=features.used))
    } else{
        return(list(data=model.data, features.unused=features.unused, features.used=features.used))
    }
}
