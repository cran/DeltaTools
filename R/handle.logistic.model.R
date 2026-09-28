#' @noRd
handle.logistic.model<-function(data, target, features, link, id.field, lam, av){
    if(av==TRUE){
        ls<-lasso.regression(data, target, features, la=lam)
        features<-ls$selected$variable
        if(dim(ls$denied)[1]>0){
            denied<-1
        } else{
            denied<-0
        }
    } else {
        denied<-0
    }
    model<-analysis.logistic.model(data0=data, target, features, link, id.field)
    return(list(model=model, features=features))
}




