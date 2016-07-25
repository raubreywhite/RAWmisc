Censor <- function(n,d=NULL){
  set.seed(4)

  index <- !is.na(n) & n >= 3
  n[index] <- n[index] + sample(c(-3:3),sum(index),replace=TRUE)
  
  index <- !is.na(n) & n < 3
  n[index] <- 0
  
  if(!is.null(d)){
    if(length(n) != length(d)) stop("n is different length to d")
    index <- !is.na(d) & d < 10
    n[index] <- 0
  }

  return(n)
}
