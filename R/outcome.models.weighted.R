#' @noRd
outcome.models.weighted<-function(datc, exp, covariates.list, out, postmatch.adjusted.model, use.weights=TRUE){
    odf<-data.frame(variable=as.character(), outcome=as.character(), coefficients=as.character(), odds_ratios=as.character(),
                    lower_ci=as.character(), upper_ci=as.character(), lower_ci_or=as.character(), upper_ci_or=as.character(),
                    standard_error=as.character(), test_stat=as.character(), pvalue=as.character(), converged=as.integer())
    if (postmatch.adjusted.model==TRUE){
      cov<-c(exp, covariates.list)
    }
    else if (postmatch.adjusted.model==FALSE){
      cov<-exp
    }
    for (i in out){
        ofom<-generate.formular.string(i, c(cov))
        if (use.weights==TRUE){
          outcome_model <- suppressWarnings(glm(formula=ofom, data=datc, weights=weights,
                                                family=binomial(link='logit')))
        } else{
          # Build linear regression model on full data
          outcome_model <- glm(formula=ofom, data=datc, family=binomial(link='logit'))
        }
        omas<-glm.get.artifacts(outcome_model)
        if (outcome_model$converged==TRUE){
            omas$outcome<-i
            omas$converged<-1
            pm1<-predict(outcome_model, type="response", newdata=datc)
            pm2<-ROCR::prediction(pm1, datc[,i])
            pm3<-ROCR::performance(pm2, "auc")
            omas$auc<-format(round(as.numeric(pm3@y.values), digits=4), nsmall=4)
        } else{
            omas$outcome<-i
            omas$converged<-0
        }
        odf<-rbind(odf, omas)
    }
    odf$flagged<-ifelse(odf$pvalue<0.05 & odf$converged==1, 1, 0)
    return(odf)
}
