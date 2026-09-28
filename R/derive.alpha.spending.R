#' @noRd
derive.alpha.spending<-function(periods, events, totals, alpha=0.05){
    a<-1/length(periods)
    b<-(alpha)/2
    t <- seq(a, 1, length=length(periods))
    ASF <- c(1,1)
    xAlpha <- c(b, b)
    quarters <- periods
    n1 <- totals
    k1 <- events
    alpha1 <- suppressWarnings(ldbounds::ldBounds(t=t,t2=n1,iuse=ASF,alpha=xAlpha))
    scoreciMEM <- function (names, x, n, zalpha, zalphaspend){
        phat <- x/n
        bound <- (zalpha * ((phat * (1 - phat) + (zalpha^2)/(4 * n))/n)^(1/2))/(1 + (zalpha^2)/n)
        boundspend <- (zalphaspend * ((phat * (1 - phat) + (zalphaspend^2)/(4 * n))/n)^(1/2))/(1 + (zalphaspend^2)/n)
        midpnt <- (phat + (zalpha^2)/(2 * n))/(1 + (zalpha^2)/n)
        midpntspend <- (phat + (zalphaspend^2)/(2 * n))/(1 + (zalphaspend^2)/n)
        uplim <- midpnt + bound
        uplimspend <- midpntspend + boundspend
        lowlim <- midpnt - bound
        lowlimspend <- midpntspend - boundspend
        cint <- c(names,x,n,lowlimspend, lowlim, phat, uplim, uplimspend,zalpha,zalphaspend)
        return(cint)
    }
    result1 <- t(mapply(scoreciMEM,quarters,k1,n1,1.96,alpha1$upper.bounds))
    colnames(result1) <- c("Name","k","n","lower_ci_spend0","lowerCI","p","upperCI","upper_ci_spend0","ciz_score0","alpha_ciz_score0")
    output<-as.data.frame(result1)
    output$lower_ci_spend<- format(round(as.numeric(as.character(output$lower_ci_spend0)), digits=4), nsmall=4)
    output$upper_ci_spend<- format(round(as.numeric(as.character(output$upper_ci_spend0)), digits=4), nsmall=4)
    output$ciz_score<- format(round(as.numeric(as.character(output$ciz_score0)), digits=4), nsmall=4)
    output$alpha_ciz_score<- format(round(as.numeric(as.character(output$alpha_ciz_score0)), digits=4), nsmall=4)
    output$period<- rownames(output)
    output<-output[c("period", "lower_ci_spend", "upper_ci_spend","ciz_score","alpha_ciz_score")]
    return(output)
}
