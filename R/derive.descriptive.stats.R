#' @noRd
derive.descriptive.stats<-function(df, fields.factors, fields.cont){
    # generate a dataframe with all possible descriptive stats per field
    # ensure that all fields are in the data
    fields<-c(fields.factors, fields.cont)
    cols<-colnames(df)
    res<-data.frame(
        variable=as.character(),
        variable_type=as.character(),
        mean=as.numeric(),
        std=as.numeric(),
        median=as.numeric(),
        min=as.numeric(),
        max=as.numeric(),
        proportion_0=as.numeric(),
        proportion_0_pct=as.numeric(),
        proportion_1=as.numeric(),
        proportion_1_pct=as.numeric(),
        q5=as.numeric(),
        q25=as.numeric(),
        q75=as.numeric(),
        q95=as.numeric(),
        skewness=as.numeric(),
        kurtosis=as.numeric(),
        standard_error=as.numeric(),
        variance=as.numeric(),
        sum=as.numeric(),
        count=as.numeric(),
        zero_count=as.integer(),
        range=as.numeric(),
        iqr=as.numeric(),
        mad=as.numeric()
    )
    if (vectors.is.contained(fields, cols)==TRUE){
        # continuous
        if (is.null(fields.cont)==FALSE){
            for (c in fields.cont){
            cdata<-df[[c]]
            cdata<-as.vector(cdata)
            c.df<-list(
                variable=c,
                variable_type='Continuous',
                mean=mean(cdata),
                std=sd(cdata),
                median=median(cdata),
                min=min(cdata),
                max=max(cdata),
                proportion_0=NA,
                proportion_1=NA,
                proportion_0_pct=NA,
                proportion_1_pct=NA,
                q5=quantile(cdata, c(0.05)),
                q25=quantile(cdata, c(0.25)),
                q75=quantile(cdata, c(0.75)),
                q95=quantile(cdata, c(0.95)),
                skewness=Skew(cdata),
                kurtosis=Kurt(cdata),
                standard_error=sd(cdata)/sqrt(length(cdata)),
                variance=var(cdata),
                sum=sum(cdata),
                count=length(cdata),
                zero_count=length(cdata[cdata==0]),
                range=max(cdata)-min(cdata),
                iqr=quantile(cdata, c(0.75))-quantile(cdata, c(0.25)),
                mad=stats::mad(cdata)
            )
            c.df<-as.data.frame(c.df)
            res<-rbind(res, c.df)
        }
        }
        # factors
        if (is.null(fields.factors)==FALSE){
            for (f in fields.factors){
            fdata<-df[[f]]
            fdata<-as.vector(fdata)
            f.df<-list(
                variable=f,
                variable_type='Factor',
                mean=NA,
                std=NA,
                median=NA,
                min=min(fdata),
                max=max(fdata),
                proportion_0=length(fdata[fdata==0]),
                proportion_1=length(fdata[fdata==1]),
                proportion_0_pct=(length(fdata[fdata==0])/length(fdata))*100,
                proportion_1_pct=(length(fdata[fdata==1])/length(fdata))*100,
                q5=NA,
                q25=NA,
                q75=NA,
                q95=NA,
                skewness=NA,
                kurtosis=NA,
                standard_error=NA,
                variance=NA,
                sum=sum(fdata),
                count=length(fdata),
                zero_count=length(fdata[fdata==0]),
                range=max(fdata)-min(fdata),
                iqr=NA,
                mad=NA
            )
            res<-rbind(res, as.data.frame(f.df))
            }
        }
        row.names(res)<-NULL
        return(list(status=TRUE, data=res, message='Passed...'))
    } else {
        return(list(
            status=FALSE,
            data=res,
            message='Some fields are missing in the data...'
        ))
    }
}
