#' @noRd
handle.desc.stats<-function(df, fields.factors, fields.cont){
    tryCatch(
        expr = {
            A<-derive.descriptive.stats(df, fields.factors, fields.cont)
            if (A$status==TRUE){
                a<-A$data
                a$mean_or_prop<-ifelse(a$variable_type=='Factor', a$proportion_1_pct, a$mean)
                a$orig_variable<-a$variable
                b<-columns.format.round(a, digits=4, cols=c('mean', 'std', 'proportion_0_pct', 'proportion_1_pct', 'skewness', 'kurtosis', 'standard_error', 'variance', 'mean_or_prop'))
                b$mean<-base::ifelse(b$variable_type=='Factor', base::round((b$proportion_1_pct/100), 4), b$mean)
                c<-b %>%
                    dplyr::select(variable, variable_type, mean, std, median, min, max, q5, q25, q75, q95, skewness, kurtosis, standard_error, variance, count, zero_count) %>%
                    dplyr::mutate(nonZeroCount=count-zero_count) %>%
                    dplyr::rename(SD=std, SE=standard_error, zeroCount=zero_count) %>%
                    dplyr::mutate(RowId=row_number()) %>%
                    dplyr::mutate_all(type.convert, as.is=TRUE) %>%
                    dplyr::select(RowId, variable, mean, SD, median, min, max, q5, q25, q75, q95, skewness, kurtosis, SE, variance, count, zeroCount, nonZeroCount) %>%
                    dplyr::rename(`Mean or Proportion`=mean,
                                  `Variable Name`=variable,
                                  Median=median,
                                  Min=min,
                                  Max=max,
                                  Skewness=skewness,
                                  Kurtosis=kurtosis,
                                  Variance=variance,
                                  `Total Count`=count,
                                  `Zero Count`=zeroCount,
                                  `Non-Zero Count`=nonZeroCount
                                  )
                return(list(status=TRUE, message='Passed...'))
            } else{
                return(list(status=FALSE, message=A$message))
            }
        },
        error = function(e){
            return(list(status=FALSE, message=e))
        },
        warning = function(w){
            return(list(status=TRUE, message=w))
        },
        finally = {
            # (Optional)
            # Do this at the end before quitting the tryCatch structure...
        }
    )
}
