#' @noRd
analysis.logistic.model<-function(data0, target, features, link, id.field){
    data0<-as.data.frame(data0)
    formular<-generate.formular.string(target, features)
    data<-data0[c(id.field, target, features)]
    # drop missing data
    data<-as.data.frame(na.omit(data))
    cases.total<-dim(data0)[1]
    cases.used<-dim(data)[1]
    cases.not.used<-cases.total-cases.used
    cases.not.used.pct<-round((cases.not.used/cases.total)*100, 2)
    case.report<-data.frame(measure='Technique', value='Logistic regression', comment='', description_id=6)
    case.report<-rbind(case.report, data.frame(measure='Link function', value=link, comment='', description_id=7))
    case.report<-rbind(case.report, data.frame(measure='Number of features', value=length(features), comment='', description_id=0))
    case.report<-rbind(case.report, data.frame(measure='Total cases available', value=cases.total, comment='', description_id=0))
    case.report<-rbind(case.report, data.frame(measure='Target', value=target, comment='', description_id=0))
    case.report<-rbind(case.report, data.frame(measure='Cases not used in the model',
                                               value=paste(cases.not.used, ' (',cases.not.used.pct, '%)', sep=''),
                                               comment='No missing data is allowed in the target, covariate and identifier fields.',
                                               description_id=0))
    model<-glm(formular, data=data, family=binomial(link = link))
    if (model$converged==FALSE){
        model.status<-list(status=FALSE, reason='Model failed to converge...')
    } else{
        coefficients<-model$coefficients
        cofs_df<-as.data.frame(coefficients)
        cofs_df_unused<-subset(cofs_df, is.na(cofs_df$coefficients))
        cofs_df<-subset(cofs_df, !is.na(cofs_df$coefficients))
        cofs_df<-subset(cofs_df, !row.names(cofs_df)=='(Intercept)')
        if (length(row.names(cofs_df))<length(features)){
            case.report<-rbind(case.report,data.frame(measure='Number of features not used',
                                                      value=length(features)-length(row.names(cofs_df)),
                                                      comment='', description_id=0))
            formular_new<-generate.formular.string(target, row.names(cofs_df))
            model<-glm(formular_new, data=data, family=binomial(link = link))
        }
        model.data<-glm.get.artifacts(model)
        model.data$comment<-''
        model.data.unused<-glm.flag.unused.features(model.data, features)
        model.data<-model.data.unused$data
        model.data$significant<-ifelse(model.data$pvalue<0.05, 1, 0)
        model.data$lower_ci_or<-as.numeric(ifelse(model.data$odds_ratios==0, NA, model.data$lower_ci_or))
        model.data$upper_ci_or<-as.numeric(ifelse(model.data$odds_ratios==0, NA, model.data$upper_ci_or))
        model.data2<-columns.format.round(model.data, 4, cols=c('coefficients','odds_ratios','lower_ci',  'upper_ci', 'lower_ci_or', 'upper_ci_or', 'standard_error', 'test_stat', 'pvalue'))
        #model density
        pscores_df<-glm.get.artifacts.pscores(model, target)
        case_id<-as.character(data[[id.field]])
        pscores_df<-cbind(pscores_df, case_id)
        pscores_df$case_id<-as.character(pscores_df$case_id)
        pscores_df$category <- ifelse(pscores_df$dependant == 1, 'Treated', 'Control')
        image.density<-plot.density.pscores.categorical(
            data=pscores_df,
            title='Density Distribution for Scores',
            x.label="Scores"
        )
        case.report<-rbind(case.report, case.report<-data.frame(measure='Minimum score', value=round(min(pscores_df$pscores), 6), comment='', description_id=0))
        case.report<-rbind(case.report, case.report<-data.frame(measure='Maximum score', value=round(max(pscores_df$pscores), 6), comment='', description_id=0))
        case.report<-rbind(case.report, case.report<-data.frame(measure='Average score', value=round(mean(pscores_df$pscores), 6), comment='', description_id=0))
        case.report<-rbind(case.report, case.report<-data.frame(measure='Standard deviation of scores', value=round(sd(pscores_df$pscores), 6), comment='', description_id=0))
        #performance
        observed<-data[[target]]
        predicted<-fitted(model)
        diagnostics0<-glm.evaluate.performance(observed, predicted)
        diagnostics<-as.data.frame(diagnostics0$measures)
        diagnostics$value<-as.numeric(diagnostics$value)
        diagnostics$description_id<-as.integer(diagnostics$description_id)
        diagnostics<-columns.format.round(diagnostics, cols=c('value'))
        diagnostics$value<-as.character(diagnostics$value)
        #eval summary
        eval_sum<-diagnostics0$evaluation_summary
        #eval detail
        eval_det<-diagnostics0$evaluation_detail
        #model estimates
        image.estimates<-plot.estimates(model=model)
        # report
        report<-data.frame(
            name=as.character(),
            value=as.character(),
            comment=as.character(),
            description_id=as.integer()
        )
        case.report <- dplyr::rename(case.report, name=measure)
        report<-rbind(case.report, diagnostics)
		# correlation
		crf<-data[c(target, features)]
		cr<-derive.correlation(crf)
		#vif
		vif<-rms::vif(model)
		vdf<-as.data.frame(vif)
		vdf$variable<-row.names(vdf)
		row.names(vdf)<-NULL
		vdf$flagged<-ifelse(vdf$vif>10, 1, 0)

        model.status<-list(status=TRUE,
                           model=model,
                           estimates=model.data2,
                           unused.features=model.data.unused$features.unused,
                           pscores=pscores_df,
                           image.estimates=image.estimates,
                           report=report,
                           evaluation_detail=eval_det,
                           evaluation_summary=eval_sum,
                           image.density=image.density,
                           confusion=diagnostics0$confusion,
						               correlation=cr,
						               vif=vdf,
						               case.report=case.report
                           )
    }
    return(model.status)
}
