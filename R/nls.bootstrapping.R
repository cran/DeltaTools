#' @noRd
nls.bootstrapping<-function(model, boots, data, form, zero_asymptote=TRUE, use.blocks=FALSE, get.cis=FALSE){
  # data must have fields-X and Y
  weibull<-function(x, a, b, c, d){a+b*base::exp(-c*x^d)}
  power<-function(x, a, b, c){a+b*x^(-c)}
  expo<-function(x, a, b, c){a+b*base::exp(-c*x)}
  logistic<-function(x, a, b, c, d){a+b/(1+c*base::exp(d*x))}
  ref<-list(
    Weibull=list(form=as.formula(Y ~ weibull(X, a, b, c, d)), np=4, starts=list(a=0, b=9.79e-08, c=4.43, d=0.239)),
    Power=list(form=as.formula(Y~power(X, a, b, c)), np=3, starts=list(a=1, b=1, c=1) ),
    Exponential=list(form=as.formula(Y~expo(X, a, b, c)), np=3, starts=list(a=1, b=1, c=1) ),
    Logistic=list(form=as.formula(Y~logistic(X, a, b, c, d)), np=4, starts=list(a=1, b=1, c=1, d=1) )
  )
  no_paras<-ref[[form]][['np']]
  fform<-ref[[form]][['form']]
  ss<-ref[[form]][['starts']]
  # new data frame of predictions
  ds<-base::subset(data, select=c('X', 'Y'))
  new_preds <- ds %>%
    do(., data.frame( X = seq(min(.$X), max(.$X), by=1), stringsAsFactors = FALSE))
  new_preds0<-new_preds
  fit0<- model
  if (zero_asymptote==TRUE){
    fa0<-fit0$m$getPars()
    fa0[['a']]<-0
    fit0$m$setPars(fa0)
  }
  new_preds0$.fitted<-predict(fit0, newdata=new_preds0)
  pdds<-data.frame(X=as.integer(), .fitted=as.numeric())
  fits<-list()
  params<-data.frame(boot_id=as.integer(), form=as.character(), term=as.character(), estimate=as.numeric(),
                     std.error=as.numeric(), statistic=as.numeric(), p.value=as.numeric())
  if (get.cis==TRUE){
    blocks<-base::seq(1, length(unique(ds$X)), 1)
    boot.seq<-base::seq(1, boots, 1)
    #print(str_glue('Bootstrapping on: {boots}'))
    for (k in boot.seq){
      if (use.blocks==TRUE){
        #create a boot dataset
        res<-data.frame(X=as.numeric(), Y=as.numeric())
        for (i in blocks){
          dsi<-base::subset(ds, X==i)
          rns<-base::rownames(dsi)
          idx<-base::sample(rns, size=length(rns), replace =T)
          dss<-dsi[idx, ]
          res<-base::rbind(res, dss)
        }
      } else {
        rns<-base::rownames(ds)
        idx<-base::sample(rns, size=length(rns), replace =T)
        res<-ds[idx, ]
      }
      # fit on the bootstrap
      a<-tryCatch({
        fit <- minpack.lm::nlsLM(fform, data=res, start = ss, control = nls.lm.control(maxiter = 1000))
        wc<-fit$convInfo$isConv
        if (wc==TRUE){
          if (zero_asymptote==TRUE){
            fa<-fit$m$getPars()
            fa[['a']]<-0
            fit$m$setPars(fa)
          }
          pms <- broom::tidy(fit)
          pms$form<-form
          pms$boot_id<-k
          params<-base::rbind(params, pms)
          new_preds$.fitted<-predict(fit, newdata=new_preds)
          pdds<-rbind(pdds, new_preds)
        }
      }, warning = function(w){
        #code that handles the warnings
      }, error = function(e){
        #code that handles the errors
      }, finally = function(f){
        #clean-up code
      })
    }
    pdds.ci<- pdds %>%
      dplyr::group_by(X) %>%
      dplyr::summarise(
        lwr_CI = quantile(.fitted, 0.025),
        upr_CI = quantile(.fitted, 0.975))
    new_preds0<-base::merge(x=new_preds0, y=pdds.ci, by='X', all.x=T)
  } else {
    new_preds0$lwr_CI<-new_preds0$.fitted
    new_preds0$upr_CI<-new_preds0$.fitted
  }
  new_preds0<-as.data.frame(new_preds0)
  fig<-plotly::plot_ly(data = as.data.frame(new_preds0), x = ~X )
  fig <- fig %>% plotly::add_trace(y = ~.fitted, name = 'Estimated Learning Curve', mode = 'lines',
                                   type='scatter', line = list(color = 'rgb(0, 0, 255)', size = 10))
  fig <- fig %>% plotly::add_trace(x=ds$X, y=ds$Y, name = 'Predicted Learning Risk', type='scatter',
                                   mode = 'markers', marker = list(color = 'rgb(17, 157, 255)', size = 1, opacity = 0.5))
  fig <- fig %>% plotly::add_trace(y = ~lwr_CI, mode = 'lines', type='scatter',
                                   line=list(color=toRGB('cornflowerblue'), width=0.1), showlegend = F)
  fig <- fig %>% plotly::add_trace(y = ~upr_CI, mode = 'lines', type='scatter',
                                   line=list(color=toRGB('cornflowerblue'), width=0.1), fill = 'tonexty', fillcolor = toRGB("cornflowerblue", 0.3), showlegend = F)
  fig <- fig %>% plotly::layout(title = 'Outcomes by experience', xaxis = list(title = 'Case Number'),
                                yaxis = list(title = 'Learning Adjustment (increased Risk)'))
  return(list(
    fig=fig,
    preds=as.data.frame(new_preds0),
    params=as.data.frame(params)
  ))
}
