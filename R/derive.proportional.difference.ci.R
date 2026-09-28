#' @noRd
derive.proportional.difference.ci<-function(pA, pB, nA, nB, alpha){
    #derive the CIs for a proportional difference
    ci_level<-1-alpha
    res <- suppressWarnings(prop.test(x = c(pA, pB), n = c(nA, nB), conf.level = ci_level))
    output<-list(
        lower_ci=format(round(res$conf.int[1], digits=4), nsmall = 4),
        upper_ci=format(round(res$conf.int[2], digits=4), nsmall = 4),
        pvalue=format(round(res$p.value, digits=4), nsmall = 4)
    )
    return(list(
        output1=output,
        output2=c(format(round(res$conf.int[1], digits=4), nsmall = 4), format(round(res$conf.int[2], digits=4), nsmall = 4), format(round(res$p.value, digits=4), nsmall = 4))))
}
