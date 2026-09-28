#' @noRd
learning.detection<-function(liuModel, liaModel, leDetectionAlpha){
  lrt<-lmtest::lrtest(liuModel, liaModel)
  lrt.df<-as.data.frame(lrt)
  lrt.pvalue<-lrt.df$`Pr(>Chisq)`[[2]]
  lrt.Chisq<-lrt.df$Chisq[[2]]
  det<-list(level='none')
  if (lrt.pvalue<leDetectionAlpha){
    det$level<-'operator'
  }
  return(
    list(
      lrTestPvalue=round(lrt.pvalue, 5),
      lrTestAlphaUsed=leDetectionAlpha,
      lrTestChisq=round(lrt.Chisq, 2),
      level=det$level
    )
  )
}
