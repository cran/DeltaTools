#' @noRd
derive.outcome.summary<-function(outcome, exposure, data, m.data, um.data){
    data<-data[c(outcome, exposure)]
    m.data<-m.data[c(outcome, exposure)]
    um.data<-um.data[c(outcome, exposure)]
    names(data)[names(data) == exposure] <- "exposure"
    names(data)[names(data) == outcome] <- "outcome"
    names(m.data)[names(m.data) == exposure] <- "exposure"
    names(m.data)[names(m.data) == outcome] <- "outcome"
    names(um.data)[names(um.data) == exposure] <- "exposure"
    names(um.data)[names(um.data) == outcome] <- "outcome"

    exposed<-data %>% dplyr::filter(exposure==1)
    nonexposed<-data %>% dplyr::filter(exposure==0)
    m.exposed<-m.data %>% dplyr::filter(exposure==1)
    m.nonexposed<-m.data %>% dplyr::filter(exposure==0)
    um.exposed<-um.data %>% dplyr::filter(exposure==1)

    exposed.outcome<-exposed %>% dplyr::filter(outcome==1)
    nonexposed.outcome<-nonexposed %>% dplyr::filter(outcome==1)
    m.exposed.outcome<-m.exposed %>% dplyr::filter(outcome==1)
    m.nonexposed.outcome<-m.nonexposed %>% dplyr::filter(outcome==1)
    um.exposed.outcome<-um.exposed %>% dplyr::filter(outcome==1)
    prior.res <- fisher.test(matrix(c(dim(exposed.outcome)[1],
                                      dim(nonexposed.outcome)[1],
                                      dim(exposed)[1] - dim(exposed.outcome)[1],
                                      dim(nonexposed)[1] - dim(nonexposed.outcome)[1]), nrow=2), conf.level=0.95)
    after.res <- fisher.test(matrix(c(dim(m.exposed.outcome)[1],
                                      dim(m.nonexposed.outcome)[1],
                                      dim(m.exposed)[1] -  dim(m.exposed.outcome)[1],
                                      dim(m.nonexposed)[1] - dim(m.nonexposed.outcome)[1]), nrow=2), conf.level=0.95)
    um.res <- fisher.test(matrix(c(dim(um.exposed.outcome)[1],
                                   dim(m.exposed.outcome)[1],
                                   dim(um.exposed)[1] - dim(um.exposed.outcome)[1],
                                   dim(m.exposed)[1] - dim(m.exposed.outcome)[1]), nrow=2), conf.level = 0.95)
    summary<-data.frame(
        outcome=outcome,
        exposure=exposure,
        prior_exposed_cases=dim(exposed)[1],
        prior_exposed_events=dim(exposed.outcome)[1],
        prior_exposed_events_percent=round( (dim(exposed.outcome)[1]/dim(exposed)[1])*100 , digits=2),
        prior_nonexposed_cases=dim(nonexposed)[1],
        prior_nonexposed_events=dim(nonexposed.outcome)[1],
        prior_nonexposed_events_percent=round( (dim(nonexposed.outcome)[1]/dim(nonexposed)[1])*100 , digits=2),
        prior_event_ratio=round( dim(exposed.outcome)[1]/ dim(nonexposed.outcome)[1],digits=2),
        prior_pvalue=round(prior.res$p.value, digits=4),

        after_exposed_cases=dim(m.exposed)[1],
        after_exposed_events=dim(m.exposed.outcome)[1],
        after_exposed_events_percent=round( (dim(m.exposed.outcome)[1]/dim(m.exposed)[1])*100 , digits=2),
        after_nonexposed_cases=dim(m.nonexposed)[1],
        after_nonexposed_events=dim(m.nonexposed.outcome)[1],
        after_nonexposed_events_percent=round( (dim(m.nonexposed.outcome)[1]/dim(m.nonexposed)[1])*100 , digits=2),
        after_event_ratio=round( dim(m.exposed.outcome)[1]/ dim(m.nonexposed.outcome)[1],digits=2),
        after_pvalue=round(after.res$p.value, digits=4),

        um_exposed_cases=dim(um.exposed)[1],
        um_exposed_events=dim(um.exposed.outcome)[1],
        um_exposed_events_percent=round( (dim(um.exposed.outcome)[1]/dim(um.exposed)[1])*100 , digits=2),
        um_event_ratio=round( dim(um.exposed.outcome)[1]/ dim(m.exposed.outcome)[1],digits=2),
        um_pvalue=round(um.res$p.value, digits=4)
    )
    return(summary)
}
