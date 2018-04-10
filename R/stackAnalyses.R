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
#' @export ExtractFits
ExtractFits <- function(fit0, fit1) {
  p_lrt <- RAWmisc::LRTest(fit0, fit1)
  res <- data.frame(coef(summary(fit1)))
  names(res) <- c("b", "se", "z", "p_wald")
  res$var <- row.names(res)
  res$n <- sum(!is.na(fit1$fitted.values))
  res$p_lrt <- p_lrt
  res <- res[, c("var", "n", "b", "se", "z", "p_wald", "p_lrt")]
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
#' @importFrom stats glm binomial gaussian coef as.formula
#' @importFrom stringr str_split
#' @import data.table
#' @export ProcessStack
ProcessStack <- function(stack, i, formatResults=FALSE) {
  if (!stack$regressionType[[i]] %in% c("logistic", "linear")) {
    stop("Non-supported regression type")
  }

  regressionType <- NULL
  a_est <- NULL
  a_b <- NULL
  a_se <- NULL
  c_est <- NULL
  c_b <- NULL
  c_se <- NULL

  if (stack$regressionType[[i]] == "logistic") {
    analysisFamily <- binomial()
    expResults <- TRUE
  } else {
    analysisFamily <- gaussian()
    expResults <- FALSE
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
  for (j in unlist(stringr::str_split(stack$exposure[[i]], "[:*]"))) {
    dataCrude <- dataCrude[!is.na(dataCrude[[j]])]
  }
  dataAdj <- copy(dataCrude)
  for (j in unlist(stringr::str_split(stack$confounders[[i]], "[:*]"))) {
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

  res_crude <- RAWmisc::ExtractFits(fit0 = fit[["crude0"]], fit1 = fit[["crude1"]])
  res_adj <- RAWmisc::ExtractFits(fit0 = fit[["adj0"]], fit1 = fit[["adj1"]])

  setnames(res_crude, c("var", "c_n", "c_b", "c_se", "c_z", "c_p_wald", "c_p_lrt"))
  setnames(res_adj, c("var", "a_n", "a_b", "a_se", "a_z", "a_p_wald", "a_p_lrt"))

  res <- merge(res_crude, res_adj, by = "var")

  res[, regressionType := stack$regressionType[[i]]]

  if (formatResults) {
    res[, a_est := RAWmisc::FormatEstCIFromEstSE(beta = a_b, se = a_se, exp = expResults)]
    res[, c_est := RAWmisc::FormatEstCIFromEstSE(beta = c_b, se = c_se, exp = expResults)]

    res <- res[res$var != "(Intercept)", c(
      "regressionType",
      "var",
      "c_n",
      "c_est",
      "c_p_wald",
      "c_p_lrt",
      "a_n",
      "a_est",
      "a_p_wald",
      "a_p_lrt"
    )]
  } else {
    res <- res[res$var != "(Intercept)", c(
      "regressionType",
      "var",
      "c_n",
      "c_b",
      "c_se",
      "c_z",
      "c_p_wald",
      "c_p_lrt",
      "a_n",
      "a_b",
      "a_se",
      "a_z",
      "a_p_wald",
      "a_p_lrt"
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
      "var",
      "c_n",
      "c_est",
      "c_p_wald",
      "c_p_lrt",
      "c_pbonf",
      "a_n",
      "a_est",
      "a_p_wald",
      "a_p_lrt",
      "a_pbonf"
    )
  } else {
    varOrder <- c(
      "regressionType",
      "var",
      "c_n",
      "c_est",
      "c_p_wald",
      "c_p_lrt",
      "a_n",
      "a_est",
      "a_p_wald",
      "a_p_lrt"
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
