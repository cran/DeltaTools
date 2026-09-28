#' @noRd
derive.correlation<-function(df, cutoff=0.8){
    # derive correlations
    a<-cor(df)
    a<-as.data.frame(a)
    z<-data.frame(x=as.character(), variable<-as.character(), corr=as.numeric())
    for (i in colnames(a)){
        b<-subset(a, row.names(a) %in% i)
        b<-as.data.frame(b)
        c<-as.data.frame(t(b))
        d<-dplyr::rename(c, corr=all_of(i))
        d$variable<-row.names(d)
        row.names(d)<-NULL
        d$x<-i
        z<-rbind(z, d)
    }
    z$y<-ifelse(z$x==z$variable, 1, 0)
    z<-subset(z, y==0)
    z$cutoff<-cutoff
    z$flagged<-ifelse(abs(z$corr)>=cutoff, 1, 0)
    z<-dplyr::arrange(z, x, desc(corr))
    z<-columns.format.round(z, cols=c('corr'))
    z<-as.data.frame(z)
    z<-z[c('x', 'variable', 'corr', 'cutoff', 'flagged')]
    return(z)
}
