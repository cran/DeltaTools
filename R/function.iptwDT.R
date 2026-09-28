#' DeltaTools Function: iptwDT
#'
#' @param data The data frame.
#' @param outcomes A character vector of one or more outcomes.
#' @param exposure A character string with the exposure.
#' @param covariates.factor A character vector of factor covariates.
#' @param covariates.continuous A character vector of continuous covariates.
#' @param identifier A character string with the record identifier.
#' @param link Specify the link function as "logit" or "probit".
#' @param avs Set to TRUE to run automatic variable selection.
#' @param lambda Specify lambda as "lambda.min" or "lambda.1se".
#' @param postmatch.adjusted.model Set to TRUE to adjust for additional variables in a logistic regression model on the matched cohort.
#' @param allow.trimming Set to TRUE to trim weights.
#' @param allow.stabilization Set to TRUE to stabilize weights.
#' @param lower.cutoff Quantile for lower cutoff to trim weights, only used if allow.trimming = TRUE. Otherwise, specify NULL.
#' @param upper.cutoff Quantile for upper cutoff to trim weights, only used if allow.trimming = TRUE. Otherwise, specify NULL.
#' @param smd.t1 Standardized mean difference threshold 1 (more conservative).
#' @param smd.t2 Standardized mean difference threshold 2 (less conservative).
#'
#' @return A list containing relevant messages about the weighting and modeling process,
#' descriptive statistics for the unweighted and weighted cohorts stratified by exposure variable,
#' covariate balance assessment, and logistic regression modeling summaries.
#' @import twang
#' @export
#'
#' @examples
#' \donttest{
#' # Inverse Probability of Treatment Weighting with automatic variable selection and
#' # trimmed, stabilized weights
#' wt <- iptwDT(data=data.ps,
#'              outcomes=c("Mortality30D", "Readmission", "RepeatProc"),
#'              exposure="DeviceB",
#'              covariates.factor=c("Aspirin", "ChrLungDis", "Diabetes", "Dialysis",
#'                                  "Emergent", "Female", "Hypertension", "NSTEMIatPresent",
#'                                  "PeripheralArterialDis", "PriorPCI", "Smoker", "Warfarin"),
#'              covariates.continuous=c("Age", "BMI", "CreatPreProc_mgdl", "FluoroMins",
#'                                      "NumVesselsIndexPCI", "TotalAdmitPCI"),
#'              identifier="Patient")
#' }
iptwDT<-function(data,
                 outcomes,
                 exposure,
                 covariates.factor,
                 covariates.continuous,
                 identifier,
                 link="logit",
                 avs=TRUE,
                 lambda="lambda.min",
                 postmatch.adjusted.model=FALSE,
                 allow.trimming=TRUE,
                 allow.stabilization=TRUE,
                 lower.cutoff=5,
                 upper.cutoff=95,
                 smd.t1=0.1,
                 smd.t2=0.25){
  messages<-vector("list", length=5)
  data0<-data
  # Treatment model
  covariates<-c(covariates.factor, covariates.continuous)
  om<-handle.logistic.model(data=data0, target=exposure, features=covariates,
                            link=link, id.field=identifier, lam=lambda, av=avs)
  if (om$model$status==TRUE){
    # Density plot PS
    pd0 <- ggplot2::ggplot(om$model$pscores, ggplot2::aes(x=pscores, fill=category)) +
      ggplot2::geom_histogram(bins=60, colour='black')
    pd1 <- pd0 + ggplot2::labs(title="Density of Propensity Scores Stratified by Exposure", x="Propensity Score", y="Count") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text=ggplot2::element_text(size=10),
                     axis.title=ggplot2::element_text(size=12)) +
      ggplot2::scale_fill_manual(name="Exposure", values=c('gray85', 'grey32'))
    print(pd1)
    messages[[1]]<-"Treatment model completed"
  } else{
    messages[[1]]<-"Treatment model failed"
  }
  # Descriptive stats
  ff<-c(covariates.factor, exposure, outcomes)
  ds<-handle.desc.stats(df=data0, fields.factors=ff, fields.cont=covariates.continuous)
  if (ds$status==TRUE){
    messages[[2]]<-"Descriptive stats completed"
	} else{
	  messages[[2]]<-"Descriptive stats failed"
  }
  # Weighting
  if (om$model$status==TRUE){
    names(data0)[names(data0) == identifier] <- "identifier"
    pscore.df<-om$model$pscores
    pscore.df$weights <- ifelse(pscore.df$category == 'Treated', (1/pscore.df$pscores), 1 / (1 - pscore.df$pscores))
    pscore.df$treatment.category<-ifelse(pscore.df$category=='Treated', 1, 0)
    pscore.df<-dplyr::rename(pscore.df, identifier=case_id)
    pscore.df<-pscore.df[c('identifier', 'treatment.category', 'pscores', 'weights')]
    pscore.df<-merge(pscore.df, data0, by='identifier')
    pscore.df[covariates.factor] <- lapply(pscore.df[covariates.factor], as.factor)
    # Stabilize weights
    if (allow.stabilization==TRUE){
      sdf<-pscore.df
      sdf.t<-base::subset(sdf, treatment.category==1)
      k.t<-mean(sdf.t$pscores)
      sdf.c<-base::subset(sdf, treatment.category==0)
      sdf.c$pscores2<-(1-sdf.c$pscores)
      k.c<-mean(sdf.c$pscores2)
      pscore.df$weights<-ifelse(pscore.df$treatment.category==1, pscore.df$weights*k.t, pscore.df$weights)
      pscore.df$weights<-ifelse(pscore.df$treatment.category==0, pscore.df$weights*k.c, pscore.df$weights)
    }
    # Trim weights
    if (allow.trimming==TRUE){
      Q05<- stats::quantile(pscore.df$weights, c(lower.cutoff/100))
      Q95 <- stats::quantile(pscore.df$weights, c(upper.cutoff/100))
      pscore.df$lower_cutoff<-Q05[[1]]
      pscore.df$upper_cutoff<-Q95[[1]]
      # Set trimmed equal to raw scores
      pscore.df$trimmed_weights<-0
      # Apply limits
      pscore.df$trimmed_weights <- ifelse(pscore.df$weights < pscore.df$lower_cutoff, pscore.df$lower_cutoff, pscore.df$weights)
      pscore.df$trimmed_weights <- ifelse(pscore.df$weights > pscore.df$upper_cutoff, pscore.df$upper_cutoff, pscore.df$weights)
      pscore.df$weights<-pscore.df$trimmed_weights
      pscore.df<-subset(pscore.df, select=c(-trimmed_weights, -lower_cutoff, -upper_cutoff ))
    }
    ps.odds <- twang::dx.wts(x=pscore.df$weights, data=pscore.df, treat.var="treatment.category", vars=om$features,
                             estimand = 'ATE', x.as.weights = TRUE)
    unweighted<-ps.odds$desc$unw
    weighted<-ps.odds$desc[[2]]
    # Unweighted
    table.uw<-as.data.frame(unweighted$bal.tab)
    table.uw<-table.uw[c('results.tx.mn', 'results.tx.sd','results.ct.mn','results.ct.sd', 'results.std.eff.sz')]
    table.uw$variable<-row.names(table.uw)
    row.names(table.uw)<-NULL
    table.uw<-dplyr::rename(table.uw, uw.tx.mn=results.tx.mn, uw.tx.sd=results.tx.sd, uw.ct.mn=results.ct.mn, uw.ct.sd=results.ct.sd,
                             uw.smd=results.std.eff.sz)
    table.uw<-columns.format.round(table.uw, digits=4, cols=c('uw.tx.mn', 'uw.tx.sd', 'uw.ct.mn', 'uw.ct.sd', 'uw.smd'))
    table.uw.sum<-data.frame(measure=as.character(), value=as.character())
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure='Unweighted Treated', value=unweighted$n.treat))
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure='Unweighted Control', value=unweighted$n.ctrl))
    # Weighted
    table.w<-as.data.frame(weighted$bal.tab)
    table.w<-table.w[c('results.tx.mn', 'results.tx.sd','results.ct.mn','results.ct.sd', 'results.std.eff.sz')]
    table.w$variable<-row.names(table.w)
    row.names(table.w)<-NULL
    table.w<-dplyr::rename(table.w, w.tx.mn=results.tx.mn, w.tx.sd=results.tx.sd, w.ct.mn=results.ct.mn, w.ct.sd=results.ct.sd,
                           w.smd=results.std.eff.sz)
    table.w<-columns.format.round(table.w, digits=4, cols=c('w.tx.mn', 'w.tx.sd', 'w.ct.mn', 'w.ct.sd', 'w.smd'))
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure='Weighted Treated', value=weighted$ess.treat))
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure='Weighted Control', value=round(weighted$ess.ctrl, digits=2)))
    table.uww<-merge(table.uw, table.w, by='variable')
    table.uww$uw.smd<-abs(as.numeric(table.uww$uw.smd))
    table.uww$w.smd<-abs(as.numeric(table.uww$w.smd))
    table.uww$flagged_before<-ifelse(table.uww$uw.smd > smd.t1, 1, 0)
    table.uww$flagged_before<-ifelse(table.uww$uw.smd > smd.t2, 2, table.uww$flagged_before)
    table.uww$flagged_after<-ifelse(table.uww$w.smd > smd.t1, 1, 0)
    table.uww$flagged_after<-ifelse(table.uww$w.smd > smd.t2, 2, table.uww$flagged_after)
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure=paste('Before weighting imbalance (>', paste(smd.t1, ')', sep=""), sep=""), value=dim(subset(table.uww, flagged_before==1))[1]))
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure=paste('Before weighting severe imbalance (>', paste(smd.t2, ')', sep=""), sep=""), value=dim(subset(table.uww, flagged_before==2))[1]))
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure=paste('After weighting imbalance (>', paste(smd.t1, ')', sep=""), sep=""), value=dim(subset(table.uww, flagged_after==1))[1]))
    table.uw.sum<-rbind(table.uw.sum, data.frame(measure=paste('After weighting severe imbalance (>', paste(smd.t2, ')', sep=""), sep=""), value=dim(subset(table.uww, flagged_after==2))[1]))
    table.uww<-dplyr::rename(table.uww, unweightedTreatedMean=uw.tx.mn, unweightedTreatedSD=uw.tx.sd, unweightedControlMean=uw.ct.mn,
                             unweightedControlSD=uw.ct.sd, unweightedSMD=uw.smd, weightedTreatedMean=w.tx.mn, weightedTreatedSD=w.tx.sd,
                             weightedControlMean=w.ct.mn, weightedControlSD=w.ct.sd, weightedSMD=w.smd)
    # Covariate report
    crp<-table.uww
    crp$isFactor<-ifelse(base::endsWith(crp$variable, ':0')==TRUE | base::endsWith(crp$variable, ':1')==TRUE, 1, 0)
    crp$iszero<-ifelse(base::endsWith(crp$variable, ':0')==TRUE, 0, 1)
    crp<-base::subset(crp, iszero==1, select=c(-iszero))
    crp$variable<-ifelse(base::endsWith(crp$variable, ':1')==TRUE, base::substr(crp$variable, 0, nchar(crp$variable)-2), crp$variable)
    crp0<-data.frame(variable='', unweightedTreatedMean=unweighted$n.treat, unweightedTreatedSD='',
                     unweightedControlMean=unweighted$n.ctrl, unweightedControlSD='', unweightedSMD='',
                     weightedTreatedMean=weighted$ess.treat, weightedTreatedSD='',
                     weightedControlMean=round(weighted$ess.ctrl, digits=2), weightedControlSD='', weightedSMD='')
    crp$noweightedControlMean<-''
    crp$noweightedControlSD<-''
    crp$noweightedSMD<-''
    crp<-base::subset(crp, select=c(-flagged_before,  -flagged_after, -isFactor,-noweightedControlMean, -noweightedControlSD, -noweightedSMD))
    crp<-base::rbind(crp0, crp)
    crp$noweightedSMD<-''
    crp<-crp %>%
        dplyr::mutate(RowId=dplyr::row_number()) %>%
        dplyr::select(RowId, variable, unweightedTreatedMean, unweightedTreatedSD, unweightedControlMean, unweightedControlSD, unweightedSMD,
                      weightedTreatedMean, weightedTreatedSD, weightedControlMean, weightedControlSD, weightedSMD)
    if (nrow(crp)>0){
      messages[[3]]<-"Covariate balancing completed"
    } else{
      messages[[3]]<-"Covariate balancing failed"
    }
    # Outcome model
    #datc<-pscore.df
    odf<-outcome.models.weighted(datc=pscore.df, exp=exposure, covariates.list=om$features,
                                 postmatch.adjusted.model=postmatch.adjusted.model, out=outcomes)
  }
  if (nrow(odf)>0 & om$model$status==TRUE){
    messages[[4]]<-"Outcome modeling completed"
    messages[[5]]<-"Weighting completed"
    return(list(weighting_stats=table.uww, covariate_stats=crp, outcome_models=odf, message=messages))
  } else if (nrow(odf==0 & om$model$status==TRUE)){
    messages[[4]]<-"Outcome modeling failed"
    messages[[5]]<-"Weighting failed"
    return(list(message=messages))
  } else{
    messages[[3]]<-"Weighting not attempted due to treatment model failed"
    return(list(message=messages))
  }
}
