#' @noRd
derive.calendar.period<-function(data, period_type){
    #user date to generate period, must have a sequencer field
    data$sequencer<-as.Date(data$sequencer, format="%Y-%m-%d")
    data$year<-format(data$sequencer, '%Y')
    #extract period from date field
    if (period_type=='year'){
        data$period<-data$year
    }
    if (period_type=='month'){
        data$period<-paste(data$year, 'M', format(data$sequencer, '%m'), sep='')
    }
    if (period_type=='quarter'){
        data$period<-paste(data$year, quarters(data$sequencer), sep='')
    }
    if (period_type=='week'){
        data$period<-format(data$sequencer, '%YW%V')
    }
    if (period_type=='half' | period_type=='half-year'){
        h1<-c('Q1', 'Q2')
        data$period<-ifelse(is.element(quarters(data$sequencer), h1), paste(data$year, 'H1', sep=''), paste(data$year, 'H2', sep=''))
    }
    return(data)
}
