#' CreateStackSkeleton
#' This creates the skeleton analysis stack
#' @param n The variable of interest
#' @export CreateStackSkeleton
CreateStackSkeleton <- function(n=1){
  s <- data.frame(regressionType=rep(NA,n))
  s$outcome <- NA
  s$exposure <- NA
  s$confounders <- NA
  s$data <- NA

  return(s)
}

#' ProcessStack
#' This processes one value from the stack
#' @param stack The stack
#' @param i The i'th stack value
#' @importFrom stats glm binomial coef
#' @import data.table
#' @export ProcessStack
ProcessStack <- function(stack,i=1){
  if(!stack$regressionType[[i]] %in% c("logistic")){
    stop("Non-supported regression type")
  }
  analysisFamily <- binomial()
  expResults <- TRUE

  form_crude <- sprintf("%s~%s",stack$outcome[[i]],stack$exposure[[i]])

  adjustedModelExists <- TRUE
  if(length(stack$confounders[[i]])==1){
    if(is.na(stack$confounders[[i]])){
      adjustedModelExists <- FALSE
    }
  }

  if(!adjustedModelExists){
    form_adj <- form_crude
  } else {
    form_adj <- sprintf("%s~%s+%s",stack$outcome[[i]],stack$exposure[[i]],paste0(stack$confounders[[i]],collapse="+"))
  }

  fit_crude <- glm(as.formula(form_crude),
                   data=get(stack$data[[i]]),
                   family=analysisFamily)
  #,na.action=na.exclude
  res_crude <- data.frame(coef(summary(fit_crude)))
  names(res_crude) <- c("c_b","c_se","c_z","c_p")
  res_crude$var <- row.names(res_crude)
  res_crude$c_n <- sum(!is.na(fit_crude$fitted.values))

  fit_adj <- glm(as.formula(form_adj),
                 data=get(stack$data[[i]]),
                 family=analysisFamily)
  #,na.action=na.exclude
  res_adj <- data.frame(coef(summary(fit_adj)))
  names(res_adj) <- c("a_b","a_se","a_z","a_p")
  res_adj$var <- row.names(res_adj)
  res_adj$a_n <- sum(!is.na(fit_adj$fitted.values))

  res <- merge(res_crude,res_adj,by="var")

  setDT(res)
  res[,a_est:=RAWmisc::FormatEstCIFromEstSE(beta=a_b,se=a_se, exp=expResults)]
  res[,c_est:=RAWmisc::FormatEstCIFromEstSE(beta=c_b,se=c_se, exp=expResults)]
  res[,regressionType:=stack$regressionType[[i]]]

  res <- res[res$var!="(Intercept)",c(
    "regressionType",
    "var",
    "c_n",
    "c_est",
    "c_p",
    "a_n",
    "a_est",
    "a_p")]

  return(res)
}

#' FormatResultsStack
#' This formats the results from the stack
#' @param results The results
#' @param bonf do we want bonferroni correction?
#' @importFrom stats glm binomial coef
#' @import data.table
#' @export FormatResultsStack
FormatResultsStack <- function(results,bonf){
  retval <- copy(results)

  if(bonf){
    retval[,c_pbonf:=c_p*.N]
    retval[,a_pbonf:=a_p*.N]
    retval[c_pbonf>1,c_pbonf:=1]
    retval[a_pbonf>1,a_pbonf:=1]

    retval[,c_sig:=ifelse(c_pbonf<0.05,"*","")]
    retval[,a_sig:=ifelse(a_pbonf<0.05,"*","")]

    retval[,c_pbonf:=RAWmisc::Format(c_pbonf,digits=3)]
    retval[,a_pbonf:=RAWmisc::Format(a_pbonf,digits=3)]
    retval[c_pbonf=="0.000",c_pbonf:="<0.001"]
    retval[a_pbonf=="0.000",a_pbonf:="<0.001"]

    varOrder <- c("regressionType",
                  "var",
                  "c_n",
                  "c_est",
                  "c_p",
                  "c_pbonf",
                  "a_n",
                  "a_est",
                  "a_p",
                  "a_pbonf")
  } else {
    retval[,c_sig:=ifelse(c_p<0.05,"*","")]
    retval[,a_sig:=ifelse(c_p<0.05,"*","")]

    varOrder <- c("regressionType",
                  "var",
                  "c_n",
                  "c_est",
                  "c_p",
                  "a_n",
                  "a_est",
                  "a_p")
  }

  retval[,c_p:=RAWmisc::Format(c_p,digits=3)]
  retval[,a_p:=RAWmisc::Format(a_p,digits=3)]
  retval[c_p=="0.000",c_p:="<0.001"]
  retval[a_p=="0.000",a_p:="<0.001"]

  retval[,c_est:=sprintf("%s%s",c_est,c_sig)]
  retval[,a_est:=sprintf("%s%s",a_est,a_sig)]

  retval[,c_sig:=NULL]
  retval[,a_sig:=NULL]

  setcolorder(retval,varOrder)

  return(retval)
}
