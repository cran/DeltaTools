#' @noRd
plot.density.pscores.categorical<-function(data, title, x.label, ...){
    graph.colors<-c("green", "blue", "red")
    return(ggplot2::ggplot(data, ggplot2::aes(x=pscores, colour=category))+
        ggplot2::scale_color_manual(values=graph.colors)+
        ggplot2::geom_density(alpha=.09)+
        ggplot2::labs(title= title, y="Density", x = x.label))
}
