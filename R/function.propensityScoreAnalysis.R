#' DeltaTools Function: propensityScoreAnalysis
#'
#' @param data The data frame.
#' @param outcomes A character vector of one or more outcomes.
#' @param exposure A character string with the exposure.
#' @param covariates.factor A character vector of factor covariates.
#' @param covariates.continuous A character vector of continuous covariates.
#' @param identifier A character string with the record identifier.
#' @param datefield The observation date if matching within a date range.
#' @param link Specify the link function as "logit" or "probit".
#' @param avs Set to TRUE to run automatic variable selection.
#' @param period.type Specifies time period as "week", "month", "quarter", "half-year", or "year"; specify "none" if not matching within a date range.
#' @param alpha The probability of a Type I error.
#' @param m.days Radius for time period, in days, if matching within a date range.
#' @param m.type Specify matching type as "closest" or "random".
#' @param lambda Specify lambda as "lambda.min" or "lambda.1se".
#' @param match.date.range Set to TRUE to match within a date range.
#' @param m.method Specify matching method as "nearest", "optimal", "full", "quick", "genetic", "cem", "exact", "cardinality", or "subclass".
#' @param m.dist Specify the distance measure as "glm", "mahalanobis", a vector of distance measures, or a matrix of pairwise distances.
#' @param m.dist.opt Specify a list of additional arguments for the function that estimates the distance measure. Otherwise, specify an empty list.
#' @param m.est Specify the target estimand as "ATT" (Average Treatment Effect on the Treated), "ATC" (Average Treatment Effect on the Controls), or "ATE" (Average Treatment Effect).
#' @param m.exact Specify a string with the variables for which exact matching should be used or specify a one-sided formula with the variables on the right-hand side. Otherwise, specify NULL.
#' @param m.mahvars Specify a string with the variables on which Mahalanobis distance matching should be used or specify a one-sided formula with the variables on the right-hand side. Otherwise, specify NULL.
#' @param m.antiexact Specify a string with the names of variables for which anti-exact matching should be used or specify a one-sided formula with the variables on the right-hand side. Otherwise, specify NULL.
#' @param m.discard Specify whether "none", "treated", "control", or "both" units should be discarded when the propensity scores fall outside the common support region.
#' @param m.reestimate Set to TRUE to re-estimate propensity scores in the sample that remains, only used if m.discard is not "none".
#' @param m.s.weights Specify a numeric vector of sampling weights for the propensity score models and balance statistics, a string containing the variable to be used, or a one-sided formula with the variable on the right-hand side. Otherwise, specify NULL.
#' @param m.replace Set to TRUE to match with replacement.
#' @param m.order Specify matching order as "largest" or "data".
#' @param m.caliper A numeric vector of caliper widths for each variable. Otherwise, NULL for no caliper.
#' @param m.ratio Specify an integer for the number of control cases to match with each treated case in k:1 matching.
#' @param m.verbose Set to TRUE to print information on the matching process to the console.
#' @param m.include.obj Set to TRUE to include objects generated during the matching process in the output.
#' @param postmatch.adjusted.model Set to TRUE to adjust for additional variables in a logistic regression model on the matched cohort.
#'
#' @return A list containing relevant messages about the matching process, summaries of unmatched and
#' matched cohort variable balance assessment, descriptive statistics, logistic regression modeling summaries,
#' proportional difference summaries, and observed vs expected summaries.
#' @importFrom caret confusionMatrix
#' @importFrom sjPlot theme_sjplot
#' @importFrom sjPlot plot_model
#' @importFrom rms vif
#' @importFrom stats glm
#' @importFrom stats predict
#' @importFrom stats sd
#' @importFrom stats binomial
#' @importFrom stats quantile
#' @importFrom stats as.formula
#' @importFrom stats resid
#' @importFrom stats coef
#' @importFrom stats model.matrix
#' @importFrom stats na.omit
#' @importFrom stats fitted
#' @importFrom stats cor
#' @importFrom stats median
#' @importFrom stats prop.test
#' @importFrom stats fisher.test
#' @importFrom stats aggregate
#' @importFrom stats residuals
#' @importFrom stats AIC
#' @importFrom methods slot
#' @importFrom data.table :=
#' @import ldbounds
#' @import DescTools
#' @import ggplot2
#' @import glmnet
#' @import MatchIt
#' @import ResourceSelection
#' @import ROCR
#' @import tableone
#' @export
#'
#' @examples
#' \donttest{
#' # Match within date range and without replacement using a standard caliper
#' ps <- propensityScoreAnalysis(data=data.ps,
#'                               outcomes=c("Mortality30D", "Readmission", "RepeatProc"),
#'                               exposure="DeviceB",
#'                               covariates.factor=c("Aspirin", "ChrLungDis", "Diabetes",
#'                                                   "Dialysis", "Emergent", "Female",
#'                                                   "Hypertension", "NSTEMIatPresent",
#'                                                   "PeripheralArterialDis", "PriorPCI",
#'                                                   "Smoker", "Warfarin"),
#'                               covariates.continuous=c("Age", "BMI", "CreatPreProc_mgdl",
#'                                                       "FluoroMins", "NumVesselsIndexPCI",
#'                                                       "TotalAdmitPCI"),
#'                               identifier="Patient",
#'                               datefield="ProcDate",
#'                               match.date.range=TRUE,
#'                               m.replace=FALSE,
#'                               m.caliper=0.2)
#' }
propensityScoreAnalysis<-function(data,
                                  outcomes,
                                  exposure,
                                  covariates.factor,
                                  covariates.continuous,
                                  identifier,
                                  datefield,
                                  link="logit",
                                  avs=TRUE,
                                  period.type="quarter",
                                  alpha=0.05,
                                  m.days=90,
                                  m.type="closest",
                                  lambda="lambda.min",
                                  match.date.range=FALSE,
                                  m.method="nearest",
                                  m.dist="glm",
                                  m.dist.opt=list(),
                                  m.est="ATT",
                                  m.exact=NULL,
                                  m.mahvars=NULL,
                                  m.antiexact=NULL,
                                  m.discard="none",
                                  m.reestimate=FALSE,
                                  m.s.weights=NULL,
                                  m.replace=FALSE,
                                  m.order="largest",
                                  m.caliper=0.01,
                                  m.ratio=1,
                                  m.verbose=FALSE,
                                  m.include.obj=FALSE,
                                  postmatch.adjusted.model=FALSE){
    ci_method<-'wilson'
    # Descriptive statistics
    ff<-c(covariates.factor, exposure, outcomes)
    ds<-handle.desc.stats(df=data, fields.factors=ff, fields.cont=covariates.continuous)
    covariates<-c(covariates.factor, covariates.continuous)
    # Treatment model
    om<-handle.logistic.model(data, target=exposure, features=covariates, link=link,
                              id.field=identifier, lam=lambda, av=avs)
    # Density plot PS
    pd0 <- ggplot2::ggplot(om$model$pscores, ggplot2::aes(x=pscores, fill=category)) +
      ggplot2::geom_histogram(bins=60, colour='black')
    pd0 <- pd0 + ggplot2::labs(title="Density of Propensity Scores Stratified by Exposure", x="Propensity Score", y="Count") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text=ggplot2::element_text(size=10),
                     axis.title=ggplot2::element_text(size=12)) +
      ggplot2::scale_fill_manual(name="Exposure", values=c('gray85', 'grey32'))
    print(pd0)
    if (om$model$status==TRUE){
        if (!is.null(m.caliper)){
          m.width<-m.caliper
        }
        else if (is.null(m.caliper)){
          m.width<-NULL
        }
        # Matching, if model succeeds
        matching<-generate.matched.cohort(dat=data, ID=identifier, target=exposure, features=covariates,
                                          MDR=match.date.range, mm=m.method, md=m.dist, ml=link, mdo=m.dist.opt,
                                          mes=m.est, mex=m.exact, mah=m.mahvars, ate=m.antiexact, mdi=m.discard,
                                          mre=m.reestimate, msw=m.s.weights, mrp=m.replace, mord=m.order, wid=m.width,
                                          rat=m.ratio, verb=m.verbose, ino=m.include.obj, dfi=datefield,
                                          radius.days=m.days)
        if(matching$message=="Success"){
            A<-covariates.factor
            B<-om$features
            C<-covariates.continuous
            dic<-as.vector(A[A %in% B])
            con<-as.vector(C[C %in% B])
            # Matching covariate balance
            balance<-propensity.assess.balance(data1=data, matched1=matching$pairs, identifier=identifier, exposed=exposure,
                                               covariates.list=om$features, covariates.fact=dic, covariates.cont=con, om1=om)
            postmatch<-handle.outcomes.postmatch(data1=data, matched1=matching$pairs, identifier=identifier, ci_method=ci_method,
                                                 exposure=exposure, outcomes=outcomes, covariates.list=om$features, sequencer=datefield,
                                                 period.type=period.type, postmatch.adjusted.model=postmatch.adjusted.model, alpha=alpha)
            # Return list with matched cohort, balance assessment, outcomes.postmatch, message "Success"
            return(list(matched_cohort=matching, balance_assessment=balance, outcomes_postmatch=postmatch, PS_Density=pd0, message="Success"))
        }
        else{
          return(list(message="Matching failed"))
        }
    }
    else{
      return(list(message="The analysis was halted due to model issues"))
    }
}
