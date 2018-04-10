#' TransformCosSinToAmplitudePeakTrough
#' Transforms cos and sin to amplitude peak and trough
#' @param cos_b The results
#' @param sin_b The results
#' @export TransformCosSinToAmplitudePeakTrough
TransformCosSinToAmplitudePeakTrough <- function(cos_b, sin_b) {
  b1 <- sin_b # sin
  b2 <- cos_b # cos

  amplitude <- sqrt(b1 ^ 2 + b2 ^ 2)
  p <- atan(b1 / b2) * 366 / 2 / pi
  if (p > 0) {
    peak <- p
    trough <- p + 366 / 2
  } else {
    peak <- p + 366 / 2
    trough <- p + 366
  }
  if (b1 < 0) {
    g <- peak
    peak <- trough
    trough <- g
  }
  return(list(
    "amplitude" = amplitude,
    "peak" = peak,
    "trough" = trough
  ))
}

#' FormatPValue
#' This formats the results from the stack
#' @param pval The results
#' @export FormatPValue
FormatPValue <- function(pval) {
  ifelse(pval < 0.001, "<0.001", RAWmisc::Format(pval, digits = 3))
}

#' LRTest
#' Likelihood ratio test wrapper
#' @param fit0 The variable of interest
#' @param fit1 The variable of interest
#' @importFrom lmtest lrtest
#' @export LRTest
LRTest <- function(fit0, fit1) {
  lmtest::lrtest(fit0, fit1)$`Pr(>Chisq)`[2]
}

#' ExtractFits
#' Extract fits
#' @param fit0 The variable of interest
#' @param fit1 The variable of interest
#' @import data.table
#' @importFrom stats AIC
#' @export ExtractFits
ExtractFits <- function(fit0, fit1) {
  p_lrt <- RAWmisc::LRTest(fit0, fit1)
  res <- data.frame(coef(summary(fit1)))
  names(res) <- c("b", "se", "z", "p_wald")
  res$exposure <- row.names(res)
  res$n <- sum(!is.na(fit1$fitted.values))
  res$p_lrt <- p_lrt
  res <- res[, c("exposure", "n", "b", "se", "z", "p_wald", "p_lrt")]
  res$aic <- AIC(fit1)
  setDT(res)
  return(res)
}

#' ExtractFitsSplines
#' Extract fits
#' @param fit0 The variable of interest
#' @param fit1 The variable of interest
#' @param stack stack
#' @param i i
#' @param data data
#' @importFrom stringr str_replace_all
#' @importFrom stats model.frame coef vcov AIC
#' @import data.table
#' @export ExtractFitsSplines
ExtractFitsSplines <- function(fit0, fit1, stack, i, data){
  sp <- NULL
  eval(parse(text=sprintf("sp <- with(data,%s)",stack$exposure)))
  dataNew0 <- data[1,]
  dataNew0[[RAWmisc::ExtractExposureConfounders(stack$exposure[[i]])]] <- 0

  dataNew1 <- data[1,]
  dataNew1[[RAWmisc::ExtractExposureConfounders(stack$exposure[[i]])]] <- 1

  newFormula <- stringr::str_replace_all(Reduce(paste, deparse(fit1$formula))," ","")
  newFormula <- stringr::str_replace_all(newFormula,"ns(\\([a-zA-Z0-9_,=]*\\))","ns\\1&&")
  newFormula <- stringr::str_replace_all(newFormula,"\\)&&",
                                         sprintf(",knots=%s,intercept=%s,Boundary.knots=%s\\)",
                                         sprintf("c(%s)",paste0(attributes(sp)$knots,collapse=",")),
                                         attributes(sp)$intercept,
                                         sprintf("c(%s)",paste0(attributes(sp)$Boundary.knots,collapse=","))
                                        ))

  m0temp <- model.frame(newFormula,data=dataNew0)
  m1temp <- model.frame(newFormula,data=dataNew1)
  m0 <- c(1)
  m1 <- c(1)
  # starts from 2 to avoid the Y, which is the first value!!
  for(j in 2:length(m0temp)){
    if(!is.factor(m0temp[[j]])){
      m0 <- c(m0,m0temp[[j]])
    } else {
      m0 <- c(m0,as.numeric(levels(m0temp[[j]])==m0temp[[j]])[-1])
    }
  }
  for(j in 2:length(m1temp)){
    if(!is.factor(m1temp[[j]])){
      m1 <- c(m1,m1temp[[j]])
    } else {
      m1 <- c(m1,as.numeric(levels(m1temp[[j]])==m1temp[[j]])[-1])
    }
  }
  changedVars <- m0!=m1
  m0 <- matrix(m0,nrow=1)
  m1 <- matrix(m1,nrow=1)

  estDif <- (m1[,changedVars]-m0[,changedVars]) %*% coef(fit1)[changedVars]

  newVCOV <- vcov(fit1)[changedVars,changedVars]
  newVCOV <- rbind(newVCOV,newVCOV)
  newVCOV <- cbind(newVCOV,newVCOV)
  m <- c(m1[,changedVars],-m0[,changedVars])

  newVar <- 0
  for(j in 1:length(m)) for(k in 1:length(m)) newVar <- newVar + m[j]*m[k]*newVCOV[j,k]

  p_lrt <- RAWmisc::LRTest(fit0, fit1)
  res <- data.frame("b"=estDif,
                    "se"=sqrt(newVar),
                    "z"=estDif/sqrt(newVar),
                    "p_wald" = RAWmisc::CalcPValue(beta=estDif,se=sqrt(newVar)))
  res$exposure <- sprintf("0 to 1, %s",stack$exposure[[i]])
  res$n <- sum(!is.na(fit1$fitted.values))
  res$p_lrt <- p_lrt
  res <- res[, c("exposure", "n", "b", "se", "z", "p_wald", "p_lrt")]
  res$aic <- AIC(fit1)
  setDT(res)
  return(res)
}

#' CreateStackSkeleton
#' This creates the skeleton analysis stack
#' @param n The variable of interest
#' @export CreateStackSkeleton
CreateStackSkeleton <- function(n=1) {
  s <- data.frame("regressionType" = rep(NA, n))
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
#' @param formatResults do you want the results formatted?
#' @importFrom stats glm binomial gaussian poisson coef as.formula
#' @importFrom stringr str_split
#' @import data.table
#' @export ProcessStack
ProcessStack <- function(stack, i, formatResults=FALSE) {
  if (!stack$regressionType[[i]] %in% c("logistic", "linear","poisson")) {
    stop("Non-supported regression type")
  }

  regressionType <- NULL
  outcome <- NULL
  a_est <- NULL
  a_b <- NULL
  a_se <- NULL
  c_est <- NULL
  c_b <- NULL
  c_se <- NULL

  if (stack$regressionType[[i]] == "logistic") {
    analysisFamily <- binomial()
    expResults <- TRUE
  } else if(stack$regressionType[[i]] == "linear"){
    analysisFamily <- gaussian()
    expResults <- FALSE
  } else if(stack$regressionType[[i]] == "poisson"){
    analysisFamily <- poisson()
    expResults <- TRUE
  }

  form_crude0 <- sprintf(
    "%s~%s",
    stack$outcome[[i]],
    1
  )
  form_crude1 <- sprintf(
    "%s~%s",
    stack$outcome[[i]],
    paste0(stack$exposure[[i]], collapse = "+")
  )

  adjustedModelExists <- TRUE
  if (length(stack$confounders[[i]]) == 1) {
    if (is.na(stack$confounders[[i]])) {
      adjustedModelExists <- FALSE
    }
  }

  if (!adjustedModelExists) {
    form_adj0 <- form_crude0
    form_adj1 <- form_crude1
  } else {
    form_adj0 <- sprintf(
      "%s~%s",
      stack$outcome[[i]],
      paste0(stack$confounders[[i]], collapse = "+")
    )
    form_adj1 <- sprintf(
      "%s~%s+%s",
      stack$outcome[[i]],
      paste0(stack$exposure[[i]], collapse = "+"),
      paste0(stack$confounders[[i]], collapse = "+")
    )
  }

  fit <- list()
  dataCrude <- copy(get(stack$data[[i]]))
  setDT(dataCrude)
  for (j in RAWmisc::ExtractExposureConfounders(stack$exposure[[i]])) {
    dataCrude <- dataCrude[!is.na(dataCrude[[j]])]
  }
  dataAdj <- copy(dataCrude)
  for (j in RAWmisc::ExtractExposureConfounders(stack$confounders[[i]])) {
    if (is.na(j)) next
    dataAdj <- dataAdj[!is.na(dataAdj[[j]])]
  }

  for (j in c("crude0", "crude1", "adj0", "adj1")) {
    if (j %in% c("crude0", "crude1")) {
      dataUse <- dataCrude
    } else {
      dataUse <- dataAdj
    }
    fit[[j]] <- glm(
      as.formula(get(sprintf("form_%s", j))),
      data = dataUse,
      family = analysisFamily
    )
  }

  if(RAWmisc::DetectSpline(stack$exposure[[i]])){
    res_crude <- RAWmisc::ExtractFitsSplines(
      fit0 = fit[["crude0"]],
      fit1 = fit[["crude1"]],
      stack = stack,
      i = i,
      data=dataCrude)
    res_adj <- RAWmisc::ExtractFitsSplines(
      fit0 = fit[["adj0"]],
      fit1 = fit[["adj1"]],
      stack = stack,
      i = i,
      data=dataAdj)
  } else {
    res_crude <- RAWmisc::ExtractFits(fit0 = fit[["crude0"]], fit1 = fit[["crude1"]])
    res_adj <- RAWmisc::ExtractFits(fit0 = fit[["adj0"]], fit1 = fit[["adj1"]])
  }

  setnames(res_crude, c("exposure", "c_n", "c_b", "c_se", "c_z", "c_p_wald", "c_p_lrt","c_aic"))
  setnames(res_adj, c("exposure", "a_n", "a_b", "a_se", "a_z", "a_p_wald", "a_p_lrt","a_aic"))

  res <- merge(res_crude, res_adj, by = "exposure")

  res[, regressionType := stack$regressionType[[i]]]
  res[, outcome := stack$outcome[[i]]]

  if (formatResults) {
    res[, a_est := RAWmisc::FormatEstCIFromEstSE(beta = a_b, se = a_se, exp = expResults)]
    res[, c_est := RAWmisc::FormatEstCIFromEstSE(beta = c_b, se = c_se, exp = expResults)]

    res <- res[res$exposure != "(Intercept)", c(
      "regressionType",
      "outcome",
      "exposure",
      "c_n",
      "c_est",
      "c_p_wald",
      "c_p_lrt",
      "c_aic",
      "a_n",
      "a_est",
      "a_p_wald",
      "a_p_lrt",
      "a_aic"
    )]
  } else {
    res <- res[res$exposure != "(Intercept)", c(
      "regressionType",
      "outcome",
      "exposure",
      "c_n",
      "c_b",
      "c_se",
      "c_z",
      "c_p_wald",
      "c_p_lrt",
      "c_aic",
      "a_n",
      "a_b",
      "a_se",
      "a_z",
      "a_p_wald",
      "a_p_lrt",
      "a_aic"
    )]
  }

  return(res)
}

#' FormatResultsStack
#' This formats the results from the stack
#' @param results The results
#' @param bonf do we want bonferroni correction?
#' @param useWald do we want significance shown on the wald pvalue?
#' @param useLRT do we want significance shown on the lrt pvalue?
#' @importFrom stats glm binomial coef
#' @import data.table
#' @export FormatResultsStack
FormatResultsStack <- function(results, bonf, useWald, useLRT) {
  if (useWald & useLRT) {
    stop("cant have both useWald and useLRT")
  }
  if (!useWald & !useLRT) {
    stop("need one of useWald and useLRT")
  }
  retval <- copy(results)

  c_pbonf <- NULL
  c_p_wald <- NULL
  a_pbonf <- NULL
  a_p_wald <- NULL
  c_p_lrt <- NULL
  a_p_lrt <- NULL
  c_sig <- NULL
  a_sig <- NULL

  a_est <- NULL
  a_b <- NULL
  a_se <- NULL
  c_est <- NULL
  c_b <- NULL
  c_se <- NULL

  if (bonf & useWald) {
    retval[, c_pbonf := stats::p.adjust(c_p_wald, method = "bonf")]
    retval[, a_pbonf := stats::p.adjust(a_p_wald, method = "bonf")]
  } else if (bonf & useLRT) {
    retval[, c_pbonf := stats::p.adjust(c_p_lrt, method = "bonf")]
    retval[, a_pbonf := stats::p.adjust(a_p_lrt, method = "bonf")]
  } else if (useWald) {
    retval[, c_sig := ifelse(c_p_wald < 0.05, "*", "")]
    retval[, a_sig := ifelse(a_p_wald < 0.05, "*", "")]
  } else if (useLRT) {
    retval[, c_sig := ifelse(c_p_lrt < 0.05, "*", "")]
    retval[, a_sig := ifelse(a_p_lrt < 0.05, "*", "")]
  }

  if (bonf) {
    retval[, c_sig := ifelse(c_pbonf < 0.05, "*", "")]
    retval[, a_sig := ifelse(a_pbonf < 0.05, "*", "")]

    retval[, c_pbonf := RAWmisc::FormatPValue(c_pbonf)]
    retval[, a_pbonf := RAWmisc::FormatPValue(a_pbonf)]

    varOrder <- c(
      "regressionType",
      "outcome",
      "exposure",
      "c_n",
      "c_est",
      "c_p_wald",
      "c_p_lrt",
      "c_pbonf",
      "c_aic",
      "a_n",
      "a_est",
      "a_p_wald",
      "a_p_lrt",
      "a_pbonf",
      "a_aic"
    )
  } else {
    varOrder <- c(
      "regressionType",
      "outcome",
      "exposure",
      "c_n",
      "c_est",
      "c_p_wald",
      "c_p_lrt",
      "c_aic",
      "a_n",
      "a_est",
      "a_p_wald",
      "a_p_lrt",
      "a_aic"
    )
  }

  retval[, c_p_wald := RAWmisc::FormatPValue(c_p_wald)]
  retval[, a_p_wald := RAWmisc::FormatPValue(a_p_wald)]

  retval[, c_p_lrt := RAWmisc::FormatPValue(c_p_lrt)]
  retval[, a_p_lrt := RAWmisc::FormatPValue(a_p_lrt)]

  retval[, c_est := sprintf("%s%s", c_est, c_sig)]
  retval[, a_est := sprintf("%s%s", a_est, a_sig)]

  retval[, c_sig := NULL]
  retval[, a_sig := NULL]

  setcolorder(retval, varOrder)

  if (bonf & useWald) {
    setnames(retval, c("c_pbonf", "a_pbonf"), c("c_p_wald_bonf", "a_p_wald_bonf"))
  } else if (bonf & useLRT) {
    setnames(retval, c("c_pbonf", "a_pbonf"), c("c_p_lrt_bonf", "a_p_lrt_bonf"))
  }

  return(retval)
}
