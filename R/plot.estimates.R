#' @noRd
plot.estimates<-function(model, ...){
    # plot model estimates
    ggplot2::theme_set(sjPlot::theme_sjplot())
    return(suppressWarnings(suppressMessages(sjPlot::plot_model(model, title= "Model Estimates", sort.est = TRUE,
                              show.values = TRUE, value.offset = .4, value.size = 3,
                              dot.size = 2))))
}
