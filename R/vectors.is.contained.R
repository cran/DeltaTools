#' @noRd
vectors.is.contained<-function(sub.set, main.set){
    # check if all the elements in the sub.set are contained in the main set
    a<-c()
    b<-unique(sub.set)
    for (s in b){
        c<-is.element(s, main.set)
        a<-append(a, c)
    }
    if (length(unique(a))==1 & a[1]==TRUE){
        return(TRUE)
    } else{
        return(FALSE)
    }
}
