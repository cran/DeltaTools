#' @noRd
derive.observed.expected.stats<-function(data, field.group, field.outcome, field.sequencer, field.identifier, period.type, alpha, ci_method){
    fields.oe <- c(field.group, field.outcome, field.sequencer, field.identifier)
    data.pd<-data %>% dplyr::select(all_of(fields.oe))
    names(data.pd)[names(data.pd) == field.group] <- "exposure"
    names(data.pd)[names(data.pd) == field.outcome] <- "outcome"
    names(data.pd)[names(data.pd) == field.sequencer] <- "sequencer"
    names(data.pd)[names(data.pd) == field.identifier] <- "ID"
    data.pd<-derive.calendar.period(data.pd, tolower(period.type))
    ex0<-stats::aggregate(data.pd$exposure, by=list(period=data.pd$period), FUN=sum)
    colnames(ex0)[2]<-'count'
    ex1<-stats::aggregate(data.pd$outcome, by=list(period=data.pd$period), FUN=sum)
    colnames(ex1)[2]<-'events'
    ex<-merge(ex0, ex1, by='period')
    ex$proportion<-format(round(ex$events/ex$count, digits=4), nsmall=4)
    ex$outcome<-field.outcome
    exb<-apply(ex, 1,
               function(x){
                   return(derive.binomial.test(yes_count=as.integer(x['events']), total_count=as.integer(x['count']),
                                               alpha=alpha, method=ci_method))
               }
    )
    ex$lower_ci<-exb[1,]
    ex$upper_ci<-exb[2,]
    ex$count_cum<-cumsum(ex$count)
    ex$events_cum<-cumsum(ex$events)
    ex$prop_cum<-format(round(ex$events_cum/ex$count_cum, digits=4), nsmall=4)
    exc<-apply(ex, 1,
               function(x){
                   return(derive.binomial.test(yes_count=as.integer(x['events_cum']), total_count=as.integer(x['count_cum']),
                                               alpha=alpha, method=ci_method))
               }
    )
    ex$lower_ci_cum<-exc[1,]
    ex$upper_ci_cum<-exc[2,]

    #alpha spending function
    asf<-derive.alpha.spending(periods=ex$period, events=ex$events_cum, totals=ex$count_cum, alpha=0.05)
    ex<-merge(ex, asf, by='period')
    return(ex)
}
