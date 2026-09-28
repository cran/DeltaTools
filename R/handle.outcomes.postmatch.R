#' @noRd
handle.outcomes.postmatch<-function(data1, matched1, identifier, ci_method, exposure, outcomes, covariates.list, sequencer, period.type,
                                    postmatch.adjusted.model, alpha){
    # Outcome analysis
    matched.caseid<-matched1$id
    matched.data0.cases<-as.data.frame(matched.caseid)
    matched.data0.cases$matched_case_id<-matched.data0.cases$matched.caseid
    matched.data0.cases<-matched.data0.cases[c('matched_case_id')]
    matched.data0.cases$matched_flag<-1
    unmatched.data<-merge(x=data1, y=matched.data0.cases, by.x=identifier, by.y='matched_case_id', all.x=TRUE)
    unmatched.data<-subset(unmatched.data, is.na(unmatched.data$matched_flag), select=-matched_flag)
    if (postmatch.adjusted.model==TRUE){
      cov<-c(exposure, covariates.list)
    }
    else if (postmatch.adjusted.model==FALSE){
      cov<-exposure
    }
    omdf<-data.frame(
                     variable=as.character(),
                     coefficients=as.numeric(),
                     odds_ratios=as.numeric(),
                     lower_ci=as.numeric(),
                     upper_ci=as.numeric(),
                     lower_ci_or=as.numeric(),
                     upper_ci_or=as.numeric(),
                     standard_error=as.numeric(),
                     test_stat=as.numeric(),
                     pvalue=as.numeric(),
                     outcome<-as.character(),
                     converge<-as.logical(),
                     auc<-as.numeric()
    )
    osum<-data.frame(
        outcome=as.character(),
        exposure=as.character(),
        prior_exposed_cases=as.numeric(),
        prior_exposed_events=as.numeric(),
        prior_exposed_events_percent=as.numeric(),
        prior_nonexposed_cases=as.numeric(),
        prior_nonexposed_events=as.numeric(),
        prior_nonexposed_events_percent=as.numeric(),
        prior_event_ratio=as.numeric(),
        prior_pvalue=as.numeric(),
        after_exposed_cases=as.numeric(),
        after_exposed_events=as.numeric(),
        after_exposed_events_percent=as.numeric(),
        after_nonexposed_cases=as.numeric(),
        after_nonexposed_events=as.numeric(),
        after_nonexposed_events_percent=as.numeric(),
        after_event_ratio=as.numeric(),
        after_pvalue=as.numeric(),
        um_exposed_cases=as.numeric(),
        um_exposed_events=as.numeric(),
        um_exposed_events_percent=as.numeric(),
        um_event_ratio=as.numeric(),
        um_pvalue=as.numeric()
    )
    pds<-data.frame(
        period=as.character(),
        pdiff_cum=as.numeric(),
        lower_ci_cum=as.numeric(),
        upper_ci_cum=as.numeric(),
        pvalue_cum=as.numeric(),
        outcome=as.character(),
        flagged=as.integer()
    )
    pdd<-data.frame(
      period=as.character(),
      exposed_count=as.integer(),
      exposed_outcomes=as.integer(),
      exposed_prop=as.numeric(),
      exposed_count.cum=as.integer(),
      exposed_outcomes_cum=as.integer(),
      exposed_cum_prop=as.numeric(),
      nonexposed_count=as.integer(),
      nonexposed_outcomes=as.integer(),
      nonexposed_prop=as.numeric(),
      nonexposed_count_cum=as.integer(),
      nonexposed_outcomes_cum=as.integer(),
      nonexposed_cum_prop=as.numeric(),
      pdiff=as.numeric(),
      pdiff_cum=as.numeric(),
      outcome=as.character(),
      lower_ci_cum=as.numeric(),
      upper_ci_cum=as.numeric(),
      pvalue_cum=as.numeric(),
      flagged=as.integer()
    )
    OE<-data.frame(
      RowId=as.integer(),
      Period_Name=as.character(),
      Observed_Count=as.integer(),
      Observed_Events=as.integer(),
      Observed_Upper_Confidence_Interval=as.numeric(),
      Observed_Lower_Confidence_Interval=as.numeric(),
      Observed_Secondary_Upper_Confidence_Interval=as.numeric(),
      Observed_Secondary_Lower_Confidence_Interval=as.numeric(),
      Observed_Proportion=as.numeric(),
      Expected_Count=as.integer(),
      Expected_Events=as.integer(),
      Expected_Upper_Confidence_Interval=as.numeric(),
      Expected_Lower_Confidence_Interval=as.numeric(),
      Expected_Secondary_Upper_Confidence_Interval=as.numeric(),
      Expected_Secondary_Lower_Confidence_Interval=as.numeric(),
      Expected_Proportion=as.numeric()
    )
    for (i in outcomes){
        fo<-generate.formular.string(i, cov)
        mo<-glm(fo, data=matched1, family = binomial(link = "logit"))
        ps<-glm.get.artifacts(mo)
        ps$outcome<-i
        pm1<-predict(mo, type="response", newdata=matched1)
        pm2<-ROCR::prediction(pm1, matched1[,i])
        pm3<-ROCR::performance(pm2, "auc")
        ps$auc<-format(round(as.numeric(pm3@y.values), digits=4), nsmall=4)
        ps$converged<-mo$converged
        omdf<-rbind(omdf, ps)
        # overall summary
        os<-derive.outcome.summary(outcome=i, exposure=exposure, data=data1, m.data=matched1, um.data=unmatched.data)
        osum<-rbind(osum, os)
        # proportional difference
        pdo<-derive.proportional.difference(data=matched1, field.group=exposure, field.outcome=i, field.sequencer=sequencer,
                                            field.identifier='id', period.type=period.type, alpha=alpha)
        ylim1 <- ifelse(as.numeric(min(pdo$detail$lower_ci_cum)) <= -0.02, as.numeric(min(pdo$detail$lower_ci_cum)),
                        as.numeric(-0.02))
        pdd<-rbind(pdd, pdo$detail)
        output <- pdo$summary
        pds<-rbind(pds, output)
        # Plot pdo$detail
        pd1 <- ggplot2::ggplot(pdo$detail,
                               ggplot2::aes(x=period, y=pdiff_cum,
                                            ymin=as.numeric(lower_ci_cum), ymax=as.numeric(upper_ci_cum))) +
          ggplot2::coord_cartesian(ylim=c(ylim1, as.numeric(max(pdo$detail$upper_ci_cum))))
        pd1 <- pd1 + ggplot2::labs(title=paste("Outcome: ", i, sep=""),
                                   subtitle="Bounds use Lan-DeMets alpha spending function method",
                                   x="Period", y="Cumulative Proportional Difference (95% CI)") +
          ggplot2::geom_pointrange() +
          ggplot2::geom_hline(yintercept=0, linetype='dotted', col='black') +
          ggplot2::theme_classic() +
          ggplot2::theme(axis.text=ggplot2::element_text(size=10),
                         axis.title=ggplot2::element_text(size=12),
                         axis.text.x=ggplot2::element_text(angle=45, hjust=1))
        print(pd1)
        pds2<-output %>%
            dplyr::select(-outcome, -flagged, -pvalue_cum) %>%
            dplyr::rename(`Period Name`=period,`Proportional Difference`=pdiff_cum,`Upper Control Limit`=upper_ci_cum,
                          `Lower Control Limit`=lower_ci_cum) %>%
            dplyr::mutate_all(type.convert, as.is=TRUE) %>%
            dplyr::mutate(RowId=dplyr::row_number()) %>%
            dplyr::select(RowId, `Period Name`,`Upper Control Limit`, `Proportional Difference`, `Lower Control Limit`)
        #observed-expected table
        matched1$exposure.ate<-1
        matched1$category<-ifelse(matched1[[exposure]]==1, 'Treated', 'Control')
        data.observed<-subset(matched1, category=='Treated')
        data.expected<-subset(matched1, category=='Control')
        observed<-derive.observed.expected.stats(data=data.observed, field.group='exposure.ate', field.outcome=i,
                                                 field.sequencer=sequencer, field.identifier='id',
                                                 period.type=period.type, alpha=alpha, ci_method=ci_method)
        observed<-observed[c('period', 'outcome', 'count_cum', 'events_cum', 'prop_cum', 'lower_ci_cum', 'upper_ci_cum',
                             'lower_ci_spend', 'upper_ci_spend')]
        observed<-dplyr::rename(observed, all_of(c("Observed_Count"="count_cum",
                                                   "Observed_Events"="events_cum",
                                                   "Observed_Proportion"="prop_cum",
                                                   "Observed_Lower_Confidence_Interval"="lower_ci_cum",
                                                   "Observed_Upper_Confidence_Interval"="upper_ci_cum",
                                                   "Observed_Secondary_Lower_Confidence_Interval"="lower_ci_spend",
                                                   "Observed_Secondary_Upper_Confidence_Interval"="upper_ci_spend")))
        expected<-derive.observed.expected.stats(data=data.expected, field.group='exposure.ate', field.outcome=i,
                                                 field.sequencer=sequencer, field.identifier='id',
                                                 period.type=period.type, alpha=alpha, ci_method)
        expected<-expected[c('period', 'count_cum', 'events_cum', 'prop_cum', 'lower_ci_cum', 'upper_ci_cum',
                             'lower_ci_spend', 'upper_ci_spend')]
        expected<-dplyr::rename(expected, all_of(c("Expected_Count"="count_cum",
                                                   "Expected_Events"="events_cum",
                                                   "Expected_Proportion"="prop_cum",
                                                   "Expected_Lower_Confidence_Interval"="lower_ci_cum",
                                                   "Expected_Upper_Confidence_Interval"="upper_ci_cum",
                                                   "Expected_Secondary_Lower_Confidence_Interval"="lower_ci_spend",
                                                   "Expected_Secondary_Upper_Confidence_Interval"="upper_ci_spend")))
        MOE<-merge(observed, expected, by='period')
        MOE<-MOE %>%
          dplyr::select(-outcome) %>%
          dplyr::mutate(RowId=dplyr::row_number()) %>%
          dplyr::mutate_all(type.convert, as.is=TRUE) %>%
          dplyr::rename(`Period_Name`=period) %>%
          dplyr::select(`RowId`, `Period_Name`,`Observed_Count`,`Observed_Events`,`Observed_Upper_Confidence_Interval`,
                        `Observed_Lower_Confidence_Interval`,`Observed_Secondary_Upper_Confidence_Interval`,
                        `Observed_Secondary_Lower_Confidence_Interval`,`Observed_Proportion`,`Expected_Count`,
                        `Expected_Events`,`Expected_Upper_Confidence_Interval`,`Expected_Lower_Confidence_Interval`,
                        `Expected_Secondary_Upper_Confidence_Interval`,`Expected_Secondary_Lower_Confidence_Interval`,
                        `Expected_Proportion`)
        OE<-rbind(OE, MOE)
    }
    omdf$flagged<-ifelse(omdf$pvalue<0.05, 1, 0)
    omdf<-omdf %>%
      dplyr::relocate(outcome)
    #outcome summary delta format
    osum <- osum %>%
      dplyr::mutate(RowId=dplyr::row_number()) %>%
      dplyr::mutate_all(type.convert, as.is=TRUE) %>%
      dplyr::select(RowId, outcome, exposure, prior_exposed_cases, prior_exposed_events_percent, prior_exposed_events, prior_nonexposed_cases, prior_nonexposed_events_percent, prior_nonexposed_events, prior_pvalue, prior_event_ratio,
                    after_exposed_cases, after_exposed_events_percent, after_exposed_events, after_nonexposed_cases, after_nonexposed_events_percent, after_nonexposed_events, after_pvalue, after_event_ratio,
                    um_exposed_cases, um_exposed_events_percent, um_exposed_events, um_pvalue, um_event_ratio)

    return(list(outcomes_models=omdf, outcomes_summary=osum, outcomes_propdiff_detail=pdd,
                outcomes_proportional_difference=pds, outcomes_observed_expected=OE))
}
