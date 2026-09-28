#' @noRd
models.generate.formular.string<-function(target, features){
  f <- as.formula(
    paste(target,
          paste(features, collapse = " + "),
          sep = " ~ "))
  return(f)
}
