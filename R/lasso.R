
LassoWithPValues <- function(x, y, family="gaussian"){
  x=scale(x,TRUE,FALSE)
  # first run glmnet
  gfit = glmnet::cv.glmnet(x,y,standardize=T, family=family)
  
  if(family=="gaussian"){
    beta = coef(gfit,se="lambda.1se",exact=TRUE)[-1]
  } else if(family=="binomial"){
    beta = coef(gfit,se="lambda.1se",exact=TRUE)
  }
  
  return(selectiveInference::fixedLassoInf(x,y,beta,gfit$lambda.1se*n,sigma=sigma,type="partial", family=family))
}

