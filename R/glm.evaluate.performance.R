#' @noRd
glm.evaluate.performance<-function(observed, predicted){
    #evaluate a model using several metrics
    #HL - performance of the model, model fits well if no significant difference between the model and observed data
    hl<-ResourceSelection::hoslem.test(observed, predicted)
    hlr<-list(
        name=hl$method,
        value=hl$p.value,
        comment=paste(ifelse(hl$p.value<0.05, 'Failed', 'Passed'), ', a threshold of 0.05 is used.', sep=''),
        description_id=1
    )
    #MSE
    d <- observed-predicted
    mse <- mean((d)^2)
    mser<-list(
        name='Mean Square Errors (MSE)',
        value=mse,
        comment='',
        description_id=2
    )
    #MAE
    mae <- mean(abs(d))

    #RMSE
    rmse <- sqrt(mse)
    rmser<-list(
        name='Root Mean Squared Errors (RMSE)',
        value=rmse,
        comment='',
        description_id=3
    )

    #R2
    r2 <- 1-(sum((d)^2)/sum((observed-mean(observed))^2))
    r2r<-list(
        name='R2 (Regression Score or Coefficient of determinant)',
        value=r2,
        comment='',
        description_id=4
    )
    #C-statistic
    cs<-DescTools::Cstat(x=predicted, resp=observed)
    csr<-list(
        name='C Statistic (Area Under the ROC Curve)',
        value=cs,
        comment=paste(ifelse(cs<0.7, 'Failed', 'Reasonable'), ', a threshold of 0.7 is used.', sep=''),
        description_id=5
    )
    # Accuracy
    tp<-ifelse(predicted>0.5, 1, 0)
    tab1<-table(Predicted=tp, Actual=observed)
    misclassification_error<-1-sum(diag(tab1))/sum(tab1)
    accuracy<-sum(diag(tab1))/sum(tab1)
    accu<-list(
        name='Accuracy',
        value=accuracy,
        comment='Threshold of 0.5 is used',
        description_id=8
    )
    mce<-list(
        name='Misclassification error rate',
        value=misclassification_error,
        comment='',
        description_id=9
    )
    pred<-factor(ifelse(predicted>0.5, 1, 0), levels=c(0,1))
    cm<-confusion.matrix(pred, observed)
    cmm <- cm$measures %>%
      dplyr::rename(name=measures,
                    value=values)
    # ROC
    p2<-ROCR::prediction(predicted, observed)
    eval<-ROCR::performance(p2, 'acc')
    max<-which.max(slot(eval, "y.values")[[1]])
    acc<-slot(eval, "y.values")[[1]][max]
    cut<-slot(eval, "x.values")[[1]][max]
    roc<-tpr<-ROCR::performance(p2, 'tpr', 'fpr')
    eval_summary<-data.frame(name=as.character(), value=as.character(), comment=as.character())
    eval_summary<-rbind(eval_summary, data.frame(name='Evaluation x-label', value=eval@x.name, comment=''))
    eval_summary<-rbind(eval_summary, data.frame(name='Evaluation y-label', value=eval@y.name, comment=''))
    eval_summary<-rbind(eval_summary, data.frame(name='Evaluation Maximum accuracy', value=round(acc, 4), comment=''))
    eval_summary<-rbind(eval_summary, data.frame(name='Evaluation Maximum cutoff', value=round(cut[[1]], 4), comment=''))
    eval_summary<-rbind(eval_summary, data.frame(name='ROC x-label', value=roc@x.name, comment=''))
    eval_summary<-rbind(eval_summary, data.frame(name='ROC y-label', value=roc@y.name, comment=''))
    eval_summary<-rbind(eval_summary, data.frame(name='ROC x-label2', value='1-Specificity', comment=''))
    eval_summary<-rbind(eval_summary, data.frame(name='ROC y-label2', value='Sensitivity', comment=''))
    evalx<-slot(eval, "x.values")[[1]] #eval@x.values
    evaly<-slot(eval, "y.values")[[1]] #eval@y.values
    rocx=slot(roc, "x.values")[[1]]#roc@x.values
    rocy=slot(roc, "y.values")[[1]] #roc@y.values
    roccut=slot(roc, "alpha.values")[[1]] #roc@alpha.values
    eval_df<-cbind(evalx, evaly, rocx, rocy, roccut)#
    eval_df<-as.data.frame(eval_df)
    eval_df<-columns.format.round(eval_df, digits=6, cols=c('evalx', 'evaly', 'rocx', 'rocy', 'roccut'))

    final<-as.data.frame(rbind(hlr, mser, rmser, r2r, csr, accu, mce))
    df <- apply(final,2,as.character)
    df<-as.data.frame(df)
    df<-rbind(df, cmm)
    return(list(measures=df, evaluation_detail=eval_df, evaluation_summary=eval_summary, confusion=cm$ctable))
}
