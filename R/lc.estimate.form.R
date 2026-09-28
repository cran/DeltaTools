#' @noRd
lc.estimate.form<-function(data, sequencer, learningRiskFieldNM, alpha=0.01, prefer.rse=TRUE){
  raw<-NA
  data$X<-data[[sequencer]]
  data$Y<-data[[learningRiskFieldNM]]
  data<-data %>% dplyr::select(X, Y)
  wc<-FALSE
  pc<-FALSE
  ec<-FALSE
  lc<-FALSE
  rc<-FALSE
  model<-NA
  #-------------------------------------------------------------------------------
  a<-tryCatch({
    weibull<-function(x, a, b, c, d){a+b*exp(-1*c*x^d)}
    wf<-as.formula(Y ~ weibull(X, a, b, c, d))
    wm <- minpack.lm::nlsLM(wf, data=data, start = list(a=1, b=1, c=1, d=1), control = nls.lm.control(maxiter = 1000))
    model<-wm
    wc<-wm$convInfo$isConv
  }, warning = function(w){
    #code that handles the warnings
  }, error = function(e){
    #code that handles the errors
  }, finally = function(f){
    #clean-up code
  })
  #-------------------------------------------------------------------------------
  a<-tryCatch({
    power<-function(x, a, b, c){a+b*x^(-1 * c)}
    pf<-as.formula(Y ~ power(X, a, b, c))
    pm <- minpack.lm::nlsLM(pf, data=data, start = list(a=1, b=1, c=1), control = nls.lm.control(maxiter = 1000))
    pc<-pm$convInfo$isConv
    model<-pm
  }, warning = function(w){
    #code that handles the warnings
  }, error = function(e){
    #code that handles the errors
  }, finally = function(f){
    #clean-up code
  })
  #-------------------------------------------------------------------------------
  a<-tryCatch({
    expo<-function(x, a, b, c){a+b*base::exp(-1 * c*x)}
    ef<-as.formula(Y ~ expo(X, a, b, c))
    em <- minpack.lm::nlsLM(ef, data=data, start = list(a=1, b=1, c=1), control = nls.lm.control(maxiter = 1000))
    ec<-em$convInfo$isConv
    model<-em
  }, warning = function(w){
    #code that handles the warnings
  }, error = function(e){
    #code that handles the errors
  }, finally = function(f){
    #clean-up code
  })
  #-------------------------------------------------------------------------------
  sdf0<-data.frame(var=as.character(), Estimate=as.numeric(), `Std. Error`=as.numeric(),
                   `Pr(>|t|)`=as.numeric(), lci=as.numeric(), uci=as.numeric(), ci=as.numeric(),
                   form=as.character(), converged=as.logical(), iterations=as.integer())
  forms<-list()
  cv<-list()
  ses<-list()
  if (pc==TRUE){
    cv$power<-TRUE
    x<-summary(pm)
    ses$power<-x$sigma
    sdf<-as.data.frame(x$coefficients)
    sdf$var<-row.names(sdf)
    sdf<-sdf[c('var', 'Estimate', 'Std. Error', 'Pr(>|t|)')]
    sdf$lci<-sdf$Estimate-sdf$`Std. Error`
    sdf$uci<-sdf$Estimate+sdf$`Std. Error`
    sdf$ci<-sdf$uci-sdf$lci
    sdf$form<-'power'
    sdf$converged<-pm$convInfo$isConv
    sdf$iterations<-pm$convInfo$finIter
    sdf$rse<-sum(resid(pm)^2)
    sdf0<-base::rbind(sdf0, sdf)
    raw<-sdf0
    forms$power<-list(
      form='a+b*x^(-1 * c)',
      name='Power',
      b=subset(sdf, var=='b')$Estimate,
      bll=subset(sdf, var=='b')$Estimate-subset(sdf, var=='b')$`Std. Error`,
      bul=subset(sdf, var=='b')$Estimate+subset(sdf, var=='b')$`Std. Error`,
      c=subset(sdf, var=='c')$Estimate,
      cll=subset(sdf, var=='c')$Estimate-subset(sdf, var=='c')$`Std. Error`,
      cul=subset(sdf, var=='c')$Estimate+subset(sdf, var=='c')$`Std. Error`,
      d=as.character(NA),
      dll=as.character(NA),
      dul=as.character(NA)
    )
  }
  if (wc==TRUE){
    cv$weibull<-TRUE
    x<-summary(wm)
    ses$weibull<-x$sigma
    sdf<-as.data.frame(x$coefficients)
    sdf$var<-row.names(sdf)
    sdf<-sdf[c('var', 'Estimate', 'Std. Error', 'Pr(>|t|)')]
    sdf$lci<-sdf$Estimate-sdf$`Std. Error`
    sdf$uci<-sdf$Estimate+sdf$`Std. Error`
    sdf$ci<-sdf$uci-sdf$lci
    sdf$form<-'weibull'
    sdf$converged<-wm$convInfo$isConv
    sdf$iterations<-wm$convInfo$finIter
    sdf$rse<-sum(resid(wm)^2)
    sdf0<-base::rbind(sdf0, sdf)
    raw<-sdf0
    forms$weibull<-list(
      form='a+b exp(-cx^d)',
      name='Weibull',
      b=subset(sdf, var=='b')$Estimate,
      bll=subset(sdf, var=='b')$Estimate-subset(sdf, var=='b')$`Std. Error`,
      bul=subset(sdf, var=='b')$Estimate+subset(sdf, var=='b')$`Std. Error`,
      c=subset(sdf, var=='c')$Estimate,
      cll=subset(sdf, var=='c')$Estimate-subset(sdf, var=='c')$`Std. Error`,
      cul=subset(sdf, var=='c')$Estimate+subset(sdf, var=='c')$`Std. Error`,
      d=subset(sdf, var=='d')$Estimate,
      dll=subset(sdf, var=='d')$Estimate-subset(sdf, var=='d')$`Std. Error`,
      dul=subset(sdf, var=='d')$Estimate+subset(sdf, var=='d')$`Std. Error`
    )
  }
  if (rc==TRUE){
    cv$reciprocal<-TRUE
    x<-summary(rm)
    ses$reciprocal<-x$sigma
    sdf<-as.data.frame(x$coefficients)
    sdf$var<-row.names(sdf)
    sdf<-sdf[c('var', 'Estimate', 'Std. Error', 'Pr(>|t|)')]
    sdf$lci<-sdf$Estimate-sdf$`Std. Error`
    sdf$uci<-sdf$Estimate+sdf$`Std. Error`
    sdf$ci<-sdf$uci-sdf$lci
    sdf$form<-'reciprocal'
    sdf$converged<-rm$convInfo$isConv
    sdf$iterations<-rm$convInfo$finIter
    sdf$rse<-sum(resid(rm)^2)
    sdf0<-base::rbind(sdf0, sdf)
    raw<-sdf0
    forms$reciprocal<-list(
      form='a+b/x',
      name='Reciprocal',
      b=subset(sdf, var=='b')$Estimate,
      bll=subset(sdf, var=='b')$Estimate-subset(sdf, var=='b')$`Std. Error`,
      bul=subset(sdf, var=='b')$Estimate+subset(sdf, var=='b')$`Std. Error`,
      c=as.character(NA), cll=as.character(NA), cul=as.character(NA),
      d=as.character(NA), dll=as.character(NA), dul=as.character(NA)
    )
  }
  if (ec==TRUE){
    cv$exponential<-TRUE
    x<-summary(em)
    ses$exponential<-x$sigma
    sdf<-as.data.frame(x$coefficients)
    sdf$var<-row.names(sdf)
    sdf<-sdf[c('var', 'Estimate', 'Std. Error', 'Pr(>|t|)')]
    sdf$lci<-sdf$Estimate-sdf$`Std. Error`
    sdf$uci<-sdf$Estimate+sdf$`Std. Error`
    sdf$ci<-sdf$uci-sdf$lci
    sdf$form<-'exponential'
    sdf$converged<-em$convInfo$isConv
    sdf$iterations<-em$convInfo$finIter
    sdf$rse<-sum(resid(em)^2)
    sdf0<-base::rbind(sdf0, sdf)
    raw<-sdf0
    forms$exponential<-list(
      form='a+b exp(-cx)',
      name='Exponential',
      b=subset(sdf, var=='b')$Estimate,
      bll=subset(sdf, var=='b')$Estimate-subset(sdf, var=='b')$`Std. Error`,
      bul=subset(sdf, var=='b')$Estimate+subset(sdf, var=='b')$`Std. Error`,
      c=subset(sdf, var=='c')$Estimate,
      cll=subset(sdf, var=='c')$Estimate-subset(sdf, var=='c')$`Std. Error`,
      cul=subset(sdf, var=='c')$Estimate+subset(sdf, var=='c')$`Std. Error`,
      d=as.character(NA),
      dll=as.character(NA),
      dul=as.character(NA)
    )
  }
  if (lc==TRUE){
    cv$logistic<-TRUE
    x<-summary(lm)
    ses$logistic<-x$sigma
    sdf<-as.data.frame(x$coefficients)
    sdf$var<-row.names(sdf)
    sdf<-sdf[c('var', 'Estimate', 'Std. Error', 'Pr(>|t|)')]
    sdf$lci<-sdf$Estimate-sdf$`Std. Error`
    sdf$uci<-sdf$Estimate+sdf$`Std. Error`
    sdf$ci<-sdf$uci-sdf$lci
    sdf$form<-'logistic'
    sdf$converged<-lm$convInfo$isConv
    sdf$iterations<-lm$convInfo$finIter
    sdf$rse<-sum(resid(lm)^2)
    sdf0<-base::rbind(sdf0, sdf)
    raw<-sdf0
    forms$logistic<-list(
      form='a+b/(1 + c exp(dx))',
      name='Logistic',
      b=subset(sdf, var=='b')$Estimate,
      bll=subset(sdf, var=='b')$Estimate-subset(sdf, var=='b')$`Std. Error`,
      bul=subset(sdf, var=='b')$Estimate+subset(sdf, var=='b')$`Std. Error`,
      c=subset(sdf, var=='c')$Estimate,
      cll=subset(sdf, var=='c')$Estimate-subset(sdf, var=='c')$`Std. Error`,
      cul=subset(sdf, var=='c')$Estimate+subset(sdf, var=='c')$`Std. Error`,
      d=subset(sdf, var=='d')$Estimate,
      dll=subset(sdf, var=='d')$Estimate-subset(sdf, var=='d')$`Std. Error`,
      dul=subset(sdf, var=='d')$Estimate+subset(sdf, var=='d')$`Std. Error`
    )
  }
  if (length(cv)>0){
    sdf0 <- sdf0 %>%
      dplyr::rename(pvalue='Pr(>|t|)')
    df<-sdf0 %>% dplyr::mutate(pvalue=base::round(pvalue, 6))
    df$pvalue_passed<-base::ifelse(df$pvalue<alpha, 1, 0)
    df$isParameter<-base::ifelse(df$var %in% c('b', 'c', 'd'), 1, 0)
    df<-base::subset(df, converged==TRUE & isParameter==1)
    df2<-df %>%
      dplyr::group_by(form) %>%
      dplyr::summarise(form_passed=sum(pvalue_passed)/sum(isParameter)) %>%
      dplyr::mutate(form_passed_highest=ifelse(form_passed==max(form_passed), 1, 0)) %>%
      dplyr::select(form, form_passed_highest, form_passed)
    df<-base::merge(x=df, y=df2, by='form', all.x=TRUE)
    df$rse<-base::round(df$rse, 2)
    #flag negative parameters
    dfn<-df
    dfn$npara<-ifelse(dfn$Estimate<0, 1, 0)
    dfn <- dfn %>%
      dplyr::group_by(form) %>%
      dplyr::summarise(flag=sum(npara)) %>%
      dplyr::mutate(npflag=ifelse(flag>0, 1, 0)) %>%
      dplyr::select(form, npflag)
    df<-base::merge(df, dfn, by='form')
    sdf01<-df
  }

  if (length(cv)==0){
    return(list(model=model, raw=raw, all=sdf0, forms=forms, convs=cv, ses=ses, estimated=0,
                selected=list(name=NA, b=NA, bll=NA, bul=NA, c=NA, cll=NA, cul=NA, d=NA, dll=NA, dul=NA)))
  } else if (length(cv)==1){
    df3<-base::subset(df, form_passed==1.0 & npflag==0)
    if (length(base::unique(df3$form))==1){
      # form with most optimized parameters is selected...
      return(list(model=model, raw=raw, all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                  selected=forms[[base::unique(df3$form)[1]]]))
    } else {
      return(list(model=model, raw=raw, all=sdf0, forms=forms, convs=cv, ses=ses, estimated=0,
                  selected=list(name=NA, b=NA, bll=NA, bul=NA, c=NA, cll=NA, cul=NA, d=NA, dll=NA, dul=NA)))
    }
    return(list(model=model, raw=raw, all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                selected=forms[[names(cv)[1]]]))
  } else if (length(cv)>1){
    df3<-base::subset(df, form_passed==1.0 & npflag==0)
    if (length(base::unique(df3$form))==1){
      # form with most optimized parameters is selected...
      return(list(model=model, raw=raw,all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                  selected=forms[[base::unique(df3$form)[1]]]))
    } else {
      # if tied, selected the one with the lowest iterations if prefer rse=FALSE
      if (prefer.rse==F){
        minIt<-base::min(df3$iterations)
        df3$hasMinIt<-base::ifelse(df3$iterations==minIt, 1, 0)
        df5<-base::subset(df3, hasMinIt==1  & npflag==0)
        if (length(base::unique(df5$form))==1){
          # form with the
          return(list(model=model, raw=raw, all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                      selected=forms[[base::unique(df5$form)[1]]]))
        } else {
          # select form with most parameters
          df6<-df5 %>%
            dplyr::group_by(form) %>%
            dplyr::summarise(mostParams=sum(isParameter))
          df5<-base::merge(x=df5, y=df6, by='form', all.x=TRUE)
          df7<-base::subset(df5, mostParams==1  & npflag==0)
          if (length(base::unique(df7$form))==1){
            return(list(model=model, raw=raw,all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                        selected=forms[[base::unique(df7$form)[1]]]))
          } else {
            return(list(model=model, raw=raw, all=sdf01, forms=forms, convs=cv, ses=ses, estimated=0,
                        selected=list(name=NA, b=NA, bll=NA, bul=NA, c=NA, cll=NA, cul=NA, d=NA, dll=NA, dul=NA)))
          }
        }
      } else {
        # give preference to form with the lowest rse and break tie with less more parameters?
        minRse<-base::min(df3$rse)
        df3$hasMinRse<-base::ifelse(df3$rse==minRse, 1, 0)
        df5<-base::subset(df3, hasMinRse==1 & npflag==0)
        if (length(base::unique(df5$form))==1){
          # form with the
          return(list(model=model, raw=raw, all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                      selected=forms[[base::unique(df5$form)[1]]]))
        } else {
          # select form with most parameters
          df6<-df5 %>%
            dplyr::group_by(form) %>%
            dplyr::summarise(mostParams=sum(isParameter))
          df5<-base::merge(x=df5, y=df6, by='form', all.x=TRUE)
          df7<-base::subset(df5, mostParams==1 & npflag==0)
          if (length(base::unique(df7$form))==1){
            return( list(model=model, raw=raw,all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                         selected=forms[[base::unique(df7$form)[1]]]))
          } else {
            # select least iterations
            minIt<-base::min(df3$iterations)
            df3$hasMinIt<-base::ifelse(df3$iterations==minIt, 1, 0)
            df5<-base::subset(df3, hasMinIt==1 & npflag==0)
            if (length(base::unique(df5$form))==1){
              # form with the
              return( list(model=model, raw=raw,all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                           selected=forms[[base::unique(df5$form)[1]]]))
            } else {
              # select form with most parameters among those with the least iterations
              df6<-df5 %>%
                dplyr::group_by(form) %>%
                dplyr::summarise(mostParams=sum(isParameter))
              df5<-base::merge(x=df5, y=df6, by='form', all.x=TRUE)
              df7<-base::subset(df5, mostParams==1 & npflag==0)
              if (length(base::unique(df7$form))==1){
                return( list(model=model, raw=raw,all=sdf01, forms=forms, convs=cv, ses=ses, estimated=1,
                             selected=forms[[base::unique(df7$form)[1]]]))
              } else {
                return( list(model=model, raw=raw,all=sdf01, forms=forms, convs=cv, ses=ses, estimated=0,
                             selected=list(name=NA, b=NA, bll=NA, bul=NA, c=NA, cll=NA, cul=NA, d=NA, dll=NA, dul=NA)))
              }
            }
          }
        }
      }
    }
  }
}
