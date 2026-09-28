#' @noRd
derive.binomial.test<-function(yes_count, total_count, alpha, method){
    #perform a binom.test
    ci.level<-1-alpha
    a<-DescTools::BinomCI(yes_count, total_count, conf.level = ci.level, method = method)
    b<-a[1,]
    c<-c(format(round(b['lwr.ci'], digits=4), nsmall=4), format(round(b['upr.ci'], digits=4), nsmall=4))
    return(c)
}
