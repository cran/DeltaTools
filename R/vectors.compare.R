#' @noRd
vectors.compare<-function(larger.vector, smaller.vector){
    #identify elements in the larger vector not in the smaller one
    A<-as.vector(larger.vector)
    B<-as.vector(smaller.vector)
    return(as.vector(A[!A %in% B]))
}
