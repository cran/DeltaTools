#' @noRd
derive.proportional.difference<-function(data, field.group, field.outcome, field.sequencer, field.identifier, period.type, alpha){
    alpha<-as.numeric(alpha)
    # calculate the proportional difference and return a df
    # exposed, nonexposed, outcome, alpha
    fields.oe <- c(field.group, field.outcome, field.sequencer, field.identifier)
    data.pd<-data %>% dplyr::select(all_of(fields.oe))
    names(data.pd)[names(data.pd) == field.group] <- "exposure"
    names(data.pd)[names(data.pd) == field.outcome] <- "outcome"
    names(data.pd)[names(data.pd) == field.sequencer] <- "sequencer"
    names(data.pd)[names(data.pd) == field.identifier] <- "ID"
    data.pd<-derive.calendar.period(data.pd, tolower(period.type))
    #groups
    exposed<-subset(data.pd, exposure==1)
    nonexposed<-subset(data.pd, exposure==0)
    #exposed
    ex0<-aggregate(exposed$exposure, by=list(periods=exposed$period), FUN=sum)
    colnames(ex0)[2]<-'exposed_count'
    ex1<-aggregate(exposed$outcome, by=list(periods=exposed$period), FUN=sum)
    colnames(ex1)[2]<-'exposed_outcomes'
    ex<-merge(ex0, ex1, by='periods')
    ex$exposed_prop<-round(ex$exposed_outcomes/ex$exposed_count, digits=4)
    #cumulative
    ex$exposed_count_cum<-cumsum(ex$exposed_count)
    ex$exposed_outcomes_cum<-cumsum(ex$exposed_outcomes)
    ex$exposed_cum_prop<-round(ex$exposed_outcomes_cum/ex$exposed_count_cum ,digits=4)

    #non-exposed
    nonexposed$exposure2=1
    nex0<-aggregate(nonexposed$exposure2, by=list(periods=nonexposed$period), FUN=sum)
    colnames(nex0)[2]<-'nonexposed_count'
    nex1<-aggregate(nonexposed$outcome, by=list(periods=nonexposed$period), FUN=sum)
    colnames(nex1)[2]<-'nonexposed_outcomes'
    nex1$nonexposed_outcomes<-round(nex1$nonexposed_outcomes, 0)
    nex<-merge(nex0, nex1, by='periods')
    nex$nonexposed_prop<-round(nex$nonexposed_outcomes/nex$nonexposed_count, digits=4)
    #cumulative
    nex$nonexposed_count_cum<-cumsum(nex$nonexposed_count)
    nex$nonexposed_outcomes_cum<-cumsum(nex$nonexposed_outcomes)
    nex$nonexposed_cum_prop<-round(nex$nonexposed_outcomes_cum/nex$nonexposed_count_cum ,digits=4)

    final<-merge(ex, nex, by='periods')
    final$pdiff<-round(abs(final$exposed_prop-final$nonexposed_prop), digits=4)
    final$pdiff_cum<-round(abs(final$exposed_cum_prop-final$nonexposed_cum_prop), digits=4)
    final$outcome<-field.outcome

    output<-final
    func<-function(a){
        b<-derive.proportional.difference.ci(pA=as.integer(a['exposed_outcomes']), pB=as.integer(a['nonexposed_outcomes']),
                                             nA=as.integer(a['exposed_count']), nB=as.integer(a['nonexposed_count']), alpha)
        return(b$output2)
    }
    func_cum<-function(a){
        b<-derive.proportional.difference.ci(pA=as.integer(a['exposed_outcomes_cum']), pB=as.integer(a['nonexposed_outcomes_cum']),
                                             nA=as.integer(a['exposed_count_cum']), nB=as.integer(a['nonexposed_count_cum']), alpha)
        return(b$output2)
    }
    # cumulative ci
    bb<-apply(output, 1, func_cum)
    output$lower_ci_cum<-bb[1,]
    output$upper_ci_cum<-bb[2,]
    output$pvalue_cum<-bb[3,]
    output$flagged<-ifelse(as.numeric(output$pvalue_cum)<alpha, 1, 0)
    output<-dplyr::rename(output, all_of(c(period="periods")))
    return(list(
        detail=output,
        summary=output[c('period', 'pdiff_cum', 'lower_ci_cum', 'upper_ci_cum', 'pvalue_cum', 'outcome', 'flagged')]
    ))
}
