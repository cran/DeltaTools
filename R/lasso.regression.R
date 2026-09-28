#' @noRd
lasso.regression<-function(df, target, features, la){
    df<-as.data.frame(df)
    df$lassotarget<-df[[target]]
    df1<-as.data.frame(df[c('lassotarget', features)])
    x = model.matrix(lassotarget~., df1)[,-1] # trim off the first column
    # leaving only the predictors
    y<-as.numeric(unlist(df1$lassotarget))
    cv.out = glmnet::cv.glmnet(x, y, alpha = 1)
    bestlam = ifelse(la=="lambda.min", cv.out$lambda.min, cv.out$lambda.1se)
    lasso_best <- glmnet::glmnet(x, y, alpha = 1, lambda = bestlam)
    ls<-coef(lasso_best)
    ls<-as.matrix(ls)
    ls<-as.data.frame(ls)
    ls$variable<-row.names(ls)
    row.names(ls)<-NULL
    predictions <- predict(lasso_best, s = bestlam, newx = x)
    s<-base::subset(ls, s0!=0 & variable!="(Intercept)")
    d<-base::subset(ls, s0==0 & variable!="(Intercept)")
    r<-list(estimates=ls, selected=s, denied=d)
    return(r)
}
