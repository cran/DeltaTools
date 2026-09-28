#' @noRd
propensity.assess.balance<-function(data1, matched1, identifier, exposed, covariates.list, covariates.fact, covariates.cont, om1){
    # assess covariate balancing using smd etc
    covariates.list<-append(covariates.list, 'pscores')
    data1$case_id<-as.character(data1[[identifier]])
    data.pscores<-om1$model$pscores
    data.pscores$case_id<-as.character(data.pscores$case_id)
    data.pscores<-data.pscores[c('case_id', 'pscores')]
    data1<-merge(data1, data.pscores, by='case_id')
    data1$category<-ifelse(data1[[exposed]]==1, 'Treated', 'Control')
    before<-derive.covariate.statistics(df=data1, covariates.all=covariates.list, covariates.fact=covariates.fact,
                                        category.field='category')
    before$stage<-'Before'

    matched1$case_id<-as.character(matched1$id)
    matched1<-merge(matched1, data.pscores, by='case_id')
    matched1$category<-ifelse(matched1[[exposed]]==1, 'Treated', 'Control')
    after<-derive.covariate.statistics(df=matched1, covariates.all=covariates.list, covariates.fact=covariates.fact,
                                       category.field='category')
    after$stage<-'After'

    data.treated<-subset(data1, category=='Treated')
    matched.treated<-data.frame(case_id = as.vector(matched1[matched1[[exposed]]==1, c('case_id')]),
                                  wasMatched = 1)
    matched.control<-data.frame(case_id = as.vector(matched1[matched1[[exposed]]==0, c('case_id')]),
                                wasMatched = 1)
    unmatched.treated<-merge(x=data.treated, y=matched.treated, by='case_id', all.x=TRUE)
    unmatched.treated$wasMatched<-ifelse(is.na(unmatched.treated$wasMatched), 0, 1)
    unmatched.treated<-subset(unmatched.treated, wasMatched==0)
    unmatched.treated.count<-dim(unmatched.treated)[1]
    unmatched.treated<-as.data.frame(unmatched.treated)
    um.grp1<-subset(matched1, category=='Treated', select=-c(category, distance, weights, subclass))
    um.grp1$pseudo.category<-'Treated.Matched'
    if(any(grepl("sequencer", colnames(um.grp1)))){
      um.grp1 <- um.grp1 %>% rename_with(~c(datefield),c(sequencer))
    }
    um.grp2<-unmatched.treated
    if(length(unmatched.treated$case_id[unmatched.treated$wasMatched==0]) > 0){
      um.grp2$pseudo.category<-'Treated.Unmatched'
    }
    um.grp2<-subset(um.grp2, select=c(-category, -wasMatched))
    # append matched treated data to unmatched and us the matched treated as the control
    names(um.grp2)[names(um.grp2) == identifier] <- "id"
    um.grp<-rbind(um.grp1, um.grp2)
    if(length(unmatched.treated$case_id[unmatched.treated$wasMatched==0]) > 0){
      um.grp.stats<-derive.covariate.statistics(df=um.grp, covariates.all=covariates.list, covariates.fact=covariates.fact,
                                                category.field='pseudo.category')
    }
    else{
      um.grp.stats<-derive.covariate.statistics(df=um.grp, covariates.all=covariates.list, covariates.fact=covariates.fact,
                                                category.field='pseudo.category', stratified=FALSE)
    }
    um.grp.stats$stage<-'Unmatched'

    balance.data<-rbind(before, after, um.grp.stats)
    balance.data<-dplyr::rename(balance.data, q25=p25, q75=p75, skewness=skew, kurtosis=kurt)
    #balance.data.raw<-balance.data
    balance.data<-columns.format.round(balance.data, 2, cols=c('mean', 'sd', 'median', 'q25', 'q75', 'min', 'max', 'skewness', 'kurtosis', 'proportion_pct'))
    balance.data<-columns.format.round(balance.data, 4, cols=c('smd'))
    balance.data$report_variable<-ifelse(balance.data$kind=='Factor', paste(balance.data$variable, '=1 (%)'),
                                         paste(balance.data$variable, '(mean (SD))'))
    balance.data$report_stats<-ifelse(balance.data$kind=='Factor', paste(balance.data$proportion, ' (', balance.data$proportion_pct ,')', sep=''),
                                      paste(balance.data$mean, ' (', balance.data$sd,')', sep='') )
    if(length(unmatched.treated$case_id[unmatched.treated$wasMatched==0]) == 0){
      p1 <- c("pscores", 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Treated.Matched", "Continuous", NA, "Unmatched", "pscores (mean (SD))", NA)
      p2 <- c("pscores", 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Treated.Unmatched", "Continuous", NA, "Unmatched", "pscores (mean (SD))", NA)
      balance.data <- rbind(balance.data, p1, p2)
      for (i in covariates.cont){
        c1 <- c(i, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Treated.Matched", "Continuous", NA, "Unmatched", paste(i, '(mean (SD))'), NA)
        c2 <- c(i, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Treated.Unmatched", "Continuous", NA, "Unmatched", paste(i, '(mean (SD))'), NA)
        balance.data <- rbind(balance.data, c1, c2)
      }
      for (i in covariates.fact){
        f1 <- c(i, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Treated.Unmatched", "Factor", NA, "Unmatched", paste(i, '=1 (%)'), NA)
        balance.data <- rbind(balance.data, f1)
      }
    }
    balance.data<-balance.data %>%
      dplyr::arrange(variable, stage, category)
    balance.data<-balance.data %>%
      dplyr::relocate(category, .after=variable)
    balance.data<-balance.data %>%
      dplyr::relocate(kind, .after=category)
    balance.data<-balance.data %>%
      dplyr::relocate(stage, .after=kind)
    # -----------------------------------------------------------------------------------------------------------------
    # mini report
    # generate a summary report
    balance.data.mini0<-balance.data[c('stage', 'category', 'variable',  'mean', 'sd', 'proportion_pct', 'smd')]
    balance.data.mini0$smd<-round(as.numeric(balance.data.mini0$smd), 2)
    balance.data.mini0$proportion_pct<-as.numeric(balance.data.mini0$proportion_pct)/100
    before.treated<-balance.data.mini0 %>%
        dplyr::filter(stage=='Before', category=='Treated') %>%
        dplyr::select('variable', 'mean', 'sd', 'proportion_pct') %>%
        dplyr::rename(bt.mean=mean, bt.sd=sd, bt.pct=proportion_pct)
    before.control<-balance.data.mini0 %>%
        dplyr::filter(stage=='Before', category=='Control') %>%
        dplyr::select('variable', 'mean', 'sd', 'proportion_pct', 'smd') %>%
        dplyr::rename(bc.mean=mean, bc.sd=sd, bc.pct=proportion_pct, before.smd=smd)
    before.treated.control<-merge(before.treated, before.control, by='variable')

    after.treated<-balance.data.mini0 %>%
        dplyr::filter(stage=='After', category=='Treated') %>%
        dplyr::select('variable',  'mean', 'sd', 'proportion_pct') %>%
        dplyr::rename(at.mean=mean, at.sd=sd, at.pct=proportion_pct)
    after.control<-balance.data.mini0 %>%
        dplyr::filter(stage=='After', category=='Control') %>%
        dplyr::select('variable',  'mean', 'sd', 'proportion_pct', 'smd') %>%
        dplyr::rename(ac.mean=mean, ac.sd=sd, ac.pct=proportion_pct, after.smd=smd)
    after.treated.control<-merge(after.treated, after.control, by='variable')

    unmatched.treated.df<-balance.data.mini0 %>%
        dplyr::filter(stage=='Unmatched', category=='Treated.Unmatched') %>%
        dplyr::select('variable',  'mean', 'sd', 'proportion_pct', 'smd') %>%
        dplyr::rename(um.mean=mean, um.sd=sd, um.pct=proportion_pct, unmatched.smd=smd)

    before.after.treated.control0<-merge(before.treated.control, after.treated.control, by='variable')
    before.after.unmatched.treated.control<-merge(before.after.treated.control0, unmatched.treated.df, by='variable')
    # -----------------------------------------------------------------------------------------------------------------
    # Generate a summary report
    balance.data.mini<-balance.data[c('stage', 'category', 'report_variable', 'report_stats', 'smd')]
    before.treated<-balance.data.mini %>%
        dplyr::filter(stage=='Before', category=='Treated') %>%
        dplyr::select('report_variable',  'report_stats') %>%
        dplyr::rename(before.treated=report_stats)
    before.control<-balance.data.mini %>%
        dplyr::filter(stage=='Before', category=='Control') %>%
        dplyr::select('report_variable',  'report_stats', 'smd') %>%
        dplyr::rename(before.control=report_stats, before.smd=smd)
    before.treated.control<-merge(before.treated, before.control, by='report_variable')

    after.treated<-balance.data.mini %>%
        dplyr::filter(stage=='After', category=='Treated') %>%
        dplyr::select('report_variable',  'report_stats') %>%
        dplyr::rename(after.treated=report_stats)
    after.control<-balance.data.mini %>%
        dplyr::filter(stage=='After', category=='Control') %>%
        dplyr::select('report_variable',  'report_stats', 'smd') %>%
        dplyr::rename(after.control=report_stats, after.smd=smd)
    after.treated.control<-merge(after.treated, after.control, by='report_variable')

    unmatched.treated.df<-balance.data.mini %>%
        dplyr::filter(stage=='Unmatched', category=='Treated.Unmatched') %>%
        dplyr::select('report_variable',  'report_stats', 'smd') %>%
        dplyr::rename(unmatched.treated=report_stats, unmatched.smd=smd)

    before.after.treated.control<-merge(before.treated.control, after.treated.control, by='report_variable')
    before.after.unmatched.treated.control<-merge(before.after.treated.control, unmatched.treated.df, by='report_variable')
    res<-data.frame(
        item=stringr::str_interp('Stratified by ${exposed}'),
        c1='',
        c2='',
        c3='',
        c4='',
        c5='',
        c6='',
        c7='',
        c8=''
    )
    r1<-data.frame(
        item='Stage',
        c1='Before',
        c2='',
        c3='',
        c4='After',
        c5='',
        c6='',
        c7='Unmatched',
        c8=''
    )
    r2<-data.frame(
        item='',
        c1='Treated',
        c2='Control',
        c3='SMD',
        c4='Treated',
        c5='Control',
        c6='SMD',
        c7='Treated',
        c8='SMD'
    )
    r3<-data.frame(
        item='n',
        c1=dim(subset(data1, category=='Treated'))[1],
        c2=dim(subset(data1, category=='Control'))[1],
        c3='',
        c4=dim(matched.treated)[1],
        c5=dim(matched.control)[1],
        c6='',
        c7=unmatched.treated.count,
        c8=''
    )
    r5<-before.after.unmatched.treated.control %>%
        dplyr::rename(item=report_variable, c1=before.treated, c2=before.control, c3=before.smd, c4=after.treated,
                      c5=after.control, c6=after.smd, c7=unmatched.treated, c8=unmatched.smd)
    r6<-subset(r5, item=='pscores (mean (SD))')
    r7<-subset(r5, item!='pscores (mean (SD))')
    res<-rbind(res, r1, r2, r3, r6, r7)
    # ------------------------------------------------------------------------------------------------------------------
    # generate a summary report
    balance.data.mini0<-balance.data[c('stage', 'category', 'variable',  'mean', 'sd', 'proportion_pct', 'smd')]
    balance.data.mini0$proportion_pct<-as.numeric(balance.data.mini0$proportion_pct)/100
    before.treated<-balance.data.mini0 %>%
        dplyr::filter(stage=='Before', category=='Treated') %>%
        dplyr::select('variable', 'mean', 'sd', 'proportion_pct') %>%
        dplyr::rename(bt.mean=mean, bt.sd=sd, bt.pct=proportion_pct)
    before.control<-balance.data.mini0 %>%
        dplyr::filter(stage=='Before', category=='Control') %>%
        dplyr::select('variable', 'mean', 'sd', 'proportion_pct', 'smd') %>%
        dplyr::rename(bc.mean=mean, bc.sd=sd, bc.pct=proportion_pct, before.smd=smd)
    before.treated.control<-merge(before.treated, before.control, by='variable')

    after.treated<-balance.data.mini0 %>%
        dplyr::filter(stage=='After', category=='Treated') %>%
        dplyr::select('variable', 'mean', 'sd', 'proportion_pct') %>%
        dplyr::rename(at.mean=mean, at.sd=sd, at.pct=proportion_pct)
    after.control<-balance.data.mini0 %>%
        dplyr::filter(stage=='After', category=='Control') %>%
        dplyr::select('variable', 'mean', 'sd', 'proportion_pct', 'smd') %>%
        dplyr::rename(ac.mean=mean, ac.sd=sd, ac.pct=proportion_pct, after.smd=smd)
    after.treated.control<-merge(after.treated, after.control, by='variable')

    unmatched.treated.df<-balance.data.mini0 %>%
        dplyr::filter(stage=='Unmatched', category=='Treated.Unmatched') %>%
        dplyr::select('variable', 'mean', 'sd', 'proportion_pct', 'smd') %>%
        dplyr::rename(um.mean=mean, um.sd=sd, um.pct=proportion_pct, unmatched.smd=smd)

    before.after.treated.control0<-merge(before.treated.control, after.treated.control, by='variable')
    before.after.unmatched.treated.control<-merge(before.after.treated.control0, unmatched.treated.df, by='variable')

    A<-before.after.unmatched.treated.control
    A$bt.mean<-ifelse(is.na(A$bt.mean)==TRUE, A$bt.pct,A$bt.mean)
    A$bc.mean<-ifelse(is.na(A$bc.mean)==TRUE, A$bc.pct,A$bc.mean)
    A$at.mean<-ifelse(is.na(A$at.mean)==TRUE, A$at.pct,A$at.mean)
    A$ac.mean<-ifelse(is.na(A$ac.mean)==TRUE, A$ac.pct,A$ac.mean)
    A$um.mean<-ifelse(is.na(A$um.mean)==TRUE, A$um.pct,A$um.mean)
    A<-subset(A, select=c('variable', 'bt.mean', 'bt.sd', 'bc.mean', 'bc.sd', 'before.smd', 'at.mean', 'at.sd', 'ac.mean',
                          'ac.sd', 'after.smd', 'um.mean', 'um.sd', 'unmatched.smd'))
    A0<-subset(res, item=='n')
    A1<-data.frame(variable='',
                   bt.mean=A0$c1,
                   bt.sd='',
                   bc.mean=A0$c2,
                   bc.sd='',
                   before.smd='',
                   at.mean=A0$c4,
                   at.sd='',
                   ac.mean=A0$c5,
                   ac.sd='',
                   after.smd='',
                   um.mean=A0$c7,
                   um.sd='',
                   unmatched.smd='')
    A<-A %>% dplyr::mutate_all(type.convert, as.is=TRUE)
    A<-columns.format.round(A, 4, cols=c('bt.mean', 'bt.sd', 'bc.mean', 'bc.sd', 'before.smd', 'at.mean', 'at.sd', 'ac.mean',
                                         'ac.sd', 'after.smd', 'um.mean', 'um.sd', 'unmatched.smd'))
    A2<-rbind(A1, A)
    A2<-A2 %>%
        dplyr::mutate(RowId=dplyr::row_number()) %>%
        dplyr::rename(bt_mean=`bt.mean`, bt_sd=`bt.sd`, bc_mean=`bc.mean`, bc_sd=`bc.sd`, b_smd=`before.smd`, at_mean=`at.mean`,
                      at_sd=`at.sd`, ac_mean=`ac.mean`, ac_sd=`ac.sd`, a_smd=`after.smd`, u_mean=`um.mean`, u_sd=`um.sd`,
                      u_smd=`unmatched.smd`) %>%
        dplyr::select(RowId, variable, bt_mean, bt_sd, bc_mean, bc_sd, b_smd, at_mean, at_sd, ac_mean, ac_sd, a_smd, u_mean,
                      u_sd, u_smd)
    try(A2<-base::subset(A2, variable!='pscores'))
    if(length(balance.data) > 0 & length(before.after.unmatched.treated.control) > 0 &
       length(res) > 0 & length(A2) > 0){
       message="Success"
    }
    else{
      message="Balance assessment failed"
    }
    return(list(matching_covariate_stats=balance.data, covariate_stats_report_mini=before.after.unmatched.treated.control,
                matching_covariate_stats_report=res, covariate_stats=A2, message=message))
}
