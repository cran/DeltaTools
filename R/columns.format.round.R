#' @noRd
columns.format.round<-function(df, digits=4, cols=NULL){
    df<-as.data.frame(df)
    #When cols=NULL, set 'cols' to all columns in data frame
    if (length(cols)==0){
        cols<-colnames(df)
    }
    for (i in cols){
        if(is.numeric(df[[i]])==TRUE){
            try(df[[i]]<-base::round(df[[i]], digits))
        }
    }
    return(df)
}


