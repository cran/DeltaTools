#' @noRd
confusion.matrix<-function(predicted, observed){
    # derive a confusion matrix & performance measures
    predicted<-base::as.factor(predicted)
    observed<-base::as.factor(observed)
    cm<-caret::confusionMatrix(predicted, observed, positive='1')
    cmt<-as.data.frame(cm$table)
    cmt$Notation<-0
    cmt$Notation[cmt$Reference==1 & cmt$Prediction==1]<-'A'
    cmt$Notation[cmt$Reference==0 & cmt$Prediction==1]<-'B'
    cmt$Notation[cmt$Reference==1 & cmt$Prediction==0]<-'C'
    cmt$Notation[cmt$Reference==0 & cmt$Prediction==0]<-'D'
    cmt <- dplyr::arrange(cmt, Notation)
    row.names(cmt)<-NULL

    values<-cm$overall
    vdf<-as.data.frame(values)
    vdf$measures<-row.names(vdf)
    row.names(vdf)<-NULL
    vdf$comment<-'overall values'
    cvalues<-cm$byClass
    cdf<-as.data.frame(cvalues)
    cdf$measures<-row.names(cdf)
    row.names(cdf)<-NULL
    cdf <- dplyr::rename(cdf, values=cvalues)
    cdf$comment<-'class values'
    vdf<-rbind(vdf, cdf)
    vdf$description_id<-0
    vdf$description_id[vdf$measures=='Sensitivity']<-11
    vdf$description_id[vdf$measures=='Specificity']<-12
    vdf$description_id[vdf$measures=='Prevalence']<-13
    vdf$description_id[vdf$measures=='Pos Pred Value']<-14
    vdf$description_id[vdf$measures=='Neg Pred Value']<-15
    vdf$description_id[vdf$measures=='Detection Rate']<-16
    vdf$description_id[vdf$measures=='Detection Prevalence']<-17
    vdf$description_id[vdf$measures=='Balanced Accuracy']<-18
    vdf$description_id[vdf$measures=='Precision']<-19
    vdf$description_id[vdf$measures=='Recall']<-20
    vdf$description_id[vdf$measures=='F1']<-21
    vdf<-columns.format.round(vdf, cols=c('values'))
    return(list(
        ctable=cmt,
        measures=vdf)
    )
}
