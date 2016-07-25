Censor <- function(n,d=NULL,boundaries=NULL){
  set.seed(4)
  
  index <- !is.na(n) & n >= 3
  new <- n
  new[index] <- new[index] + sample(c(-3:3),sum(index),replace=TRUE)
  if(!is.null(boundaries)){
    if(!is.list(boundaries)) boundaries <- list(boundaries)
    for(i in 1:length(boundaries)){
      index <- new >= boundaries[[i]] & n < boundaries[[i]]
      new[index] <- boundaries[[i]][index] - 1
    }
  }
  n <- new
  
  index <- !is.na(n) & n < 3
  n[index] <- 0
  
  if(!is.null(d)){
    if(length(n) != length(d)) stop("n is different length to d")
    index <- !is.na(d) & d < 10
    n[index] <- 0
  }
  
  return(n)
}
