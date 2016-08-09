Censor <- function(n,randomNoise,d=NULL,boundaries=NULL){
  set.seed(4)
  
  index <- n >= 3
  index[is.na(index)] <- FALSE
  new <- n
  new[index] <- new[index] + randomNoise
  if(!is.null(boundaries)){
    if(!is.list(boundaries)) boundaries <- list(boundaries)
    for(i in 1:length(boundaries)){
      index <- new >= boundaries[[i]] & n < boundaries[[i]]
      index[is.na(index)] <- FALSE
      new[index] <- boundaries[[i]][index] - 1
      
      index <- new < boundaries[[i]] & n >= boundaries[[i]]
      index[is.na(index)] <- FALSE
      new[index] <- boundaries[[i]][index] + 0.001
    }
  }
  n <- new
  
  index <- n < 3
  index[is.na(index)] <- FALSE
  n[index] <- 0
  
  if(!is.null(d)){
    if(length(n) != length(d)) stop("n is different length to d")
    index <- d < 10
    index[is.na(index)] <- FALSE
    n[index] <- 0
  }
  
  return(n)
}
