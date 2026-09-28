#' @noRd
derive.covariate.statistics<-function(df, covariates.all, covariates.fact, category.field, stratified=TRUE){
    if(stratified){
      tab <- tableone::CreateTableOne(vars = covariates.all, factorVars=covariates.fact, strata = category.field, data = df, test = FALSE, smd=TRUE)
    }
    else{
      tab <- tableone::CreateTableOne(vars = covariates.all, factorVars=covariates.fact, data = df, test = FALSE, smd=TRUE)
    }
    cats<-unique(df[[category.field]])
    res<-data.frame(
        variable=as.character(),
        total=as.integer(),
        missing=as.integer(),
        missing_proportion=as.numeric(),
        mean=as.numeric(),
        sd=as.numeric(),
        median=as.numeric(),
        p25=as.numeric(),
        p75=as.numeric(),
        min=as.numeric(),
        max=as.numeric(),
        skew=as.numeric(),
        kurt=as.numeric(),
        category=as.character(),
        proportion=as.numeric(),
        proportion_pct=as.numeric(),
        kind=as.character()
    )
    for (c in cats){
        #cont
        covariates.stats.cont<-as.data.frame(tab$ContTable[[c]])
        if (dim(covariates.stats.cont)[1] > 0){
            covariates.stats.cont$variable<-row.names(covariates.stats.cont)
            covariates.stats.cont$proportion<-NA
            covariates.stats.cont$proportion_pct<-NA
            covariates.stats.cont$category<-c
            covariates.stats.cont$kind<-'Continuous'
            covariates.stats.cont <- dplyr::rename(covariates.stats.cont, total=n, missing=miss, missing_proportion=p.miss)
            res<-rbind(res, covariates.stats.cont)
        }
        #cats
        if (length(covariates.fact)>0){
            cat.df<-data.frame(n=as.integer(), miss=as.integer(), p.miss=as.numeric(), level=as.integer(), freq=as.integer(),
                               percent=as.numeric(), cum.percent=as.numeric(), variable=as.character())
            if(dim(tab$CatTable) > 1){
              for (f in covariates.fact){
                fr<-tab$CatTable[[c]][[f]]
                fdf<-as.data.frame(fr)
                fdf$variable<-f
                cat.df<-rbind(cat.df, fdf)
              }
            }
            else{
              for (f in covariates.fact){
                fr<-tab$CatTable[[1]][[f]]
                fdf<-as.data.frame(fr)
                fdf$variable<-f
                cat.df<-rbind(cat.df, fdf)
              }
            }
            cat.df.one<-subset(cat.df, level==1)
            cat.df.one <- dplyr::rename(cat.df.one, total=n, missing=miss, missing_proportion=p.miss, proportion=freq,
                                        proportion_pct=percent)
            cat.df.one<-cat.df.one[c('variable', 'total', 'missing', 'missing_proportion', 'proportion', 'proportion_pct')]
            cat.df.one$mean<-NA
            cat.df.one$sd<-NA
            cat.df.one$median<-NA
            cat.df.one$p25<-NA
            cat.df.one$p75<-NA
            cat.df.one$min<-NA
            cat.df.one$max<-NA
            cat.df.one$skew<-NA
            cat.df.one$kurt<-NA
            cat.df.one$category<-c
            cat.df.one$kind<-'Factor'
            res<-rbind(res, cat.df.one)
        }
    }
    if(dim(tab$CatTable) > 1){
      smd<-rbind(attr(tab$ContTable, 'smd'), attr(tab$CatTable, 'smd'))
      smd<-as.data.frame(smd)
      smd$variable<-row.names(smd)
      smd <- dplyr::rename(smd, smd='1 vs 2')
      row.names(smd)<-NULL
      res<-merge(res, smd, by='variable')
    }
    else{
      res$smd<-NA
    }
    res<-columns.format.round(res, 4)
    return(res)
}
