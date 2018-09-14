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
#' @param fit1aic with a possibly more friendly family (possion instead of quasipoisson)
#' @param exposureValue exposureValue
#' @param nameBase base level for interaction terms
#' @param nameInteractions interaction variables
#' @import data.table
#' @importFrom stats AIC
#' @export ExtractFits
ExtractFits <- function(fit0, fit1, fit1aic, exposureValue=1, nameBase=NULL, nameInteractions=NULL) {
  p_lrt <- RAWmisc::LRTest(fit0, fit1)
  res <- data.frame(coef(summary(fit1)))
  names(res) <- c("b", "se", "z", "p_wald")
  res$exposure <- row.names(res)
  res$n <- sum(!is.na(fit1$fitted.values))
  res$p_lrt <- p_lrt
  res <- res[, c("exposure", "n", "b", "se", "z", "p_wald", "p_lrt")]

  if(!is.null(nameBase) & !is.null(nameInteractions)) if(!is.na(nameBase) & !is.na(nameInteractions)){
    temp <- ExtractInteractedEffectEstimates(
      beta=coef(fit1),
      va=vcov(fit1),
      nameBase=nameBase,
      nameInteractions=nameInteractions)
    temp <- data.frame(temp)
    temp$exposure <- c(sprintf("COMBINATION: %s",nameBase),
                       sprintf("COMBINATION: %s + %s",nameBase,nameInteractions)
                      )
    temp$n <- NA
    temp$z <- NA
    temp$p_lrt <- NA
    temp <- temp[,c("exposure","n","beta","se","z","p","p_lrt")]
    names(temp) <- c("exposure","n","b","se","z","p_wald","p_lrt")
    res <- rbind(res,temp)
  }

  res$b <- res$b*exposureValue
  res$se <- res$se*exposureValue
  res$aic <- AIC(fit1aic)


  setDT(res)
  return(res)
}

#' ExtractFitsSplines
#' Extract fits
#' @param fit0 The variable of interest
#' @param fit1 The variable of interest
#' @param fit1aic with a possibly more friendly family (possion instead of quasipoisson)
#' @param stack stack
#' @param i i
#' @param form the formula
#' @param data data
#' @param exposureValue exposureValue
#' @param nameInteractions x
#' @param levelInteractions x
#' @importFrom stringr str_replace_all str_extract
#' @importFrom stats model.matrix coef vcov AIC
#' @importFrom splines ns
#' @import data.table
#' @export ExtractFitsSplines
ExtractFitsSplines <- function(fit0, fit1, fit1aic, stack, i, data, form, exposureValue=1, nameInteractions=NULL, levelInteractions=NULL){

  splineExposure <- stringr::str_extract(stack$exposure[[i]],"ns(\\([a-zA-Z0-9_,=]*\\))")
  sp <- NULL
  eval(parse(text=sprintf("sp <- with(data,splines::%s)",splineExposure)))

  exposureSpline <- stringr::str_split(stack$exposure[[i]],":")[[1]]
  exposureSpline <- exposureSpline[stringr::str_detect(exposureSpline,"^ns\\(")]
  dataNew0 <- data[1,]
  for(j in exposureSpline){
    if(DetectSpline(j)) dataNew0[[RAWmisc::ExtractExposureConfounders(j)]] <- 0
  }

  dataNew1 <- data[1,]
  for(j in exposureSpline){
    if(DetectSpline(j)) dataNew1[[RAWmisc::ExtractExposureConfounders(j)]] <- 1
  }
  if(!is.null(nameInteractions) & !is.null(levelInteractions)) if(!is.na(nameInteractions) & !is.na(levelInteractions)){
    dataNew0[[nameInteractions]] <- levelInteractions
    dataNew1[[nameInteractions]] <- levelInteractions
  }

  newFormula <- stringr::str_replace_all(form," ","")
  newFormula <- stringr::str_replace_all(newFormula,"ns(\\([a-zA-Z0-9_,=]*\\))","ns\\1&&")
  newFormula <- stringr::str_replace_all(newFormula,"\\)&&",
                                         sprintf(",knots=%s,intercept=%s,Boundary.knots=%s\\)",
                                         sprintf("c(%s)",paste0(attributes(sp)$knots,collapse=",")),
                                         attributes(sp)$intercept,
                                         sprintf("c(%s)",paste0(attributes(sp)$Boundary.knots,collapse=","))
                                        ))

  #m0temp <- model.frame(newFormula,data=dataNew0)
  #m1temp <- model.frame(newFormula,data=dataNew1)
  m0temp <- model.matrix(as.formula(newFormula),data=dataNew0)
  m1temp <- model.matrix(as.formula(newFormula),data=dataNew1)
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
  res$exposure <- sprintf("0 to %s, %s",exposureValue,stack$exposure[[i]])
  res$n <- sum(!is.na(fit1$fitted.values))
  res$p_lrt <- p_lrt
  res <- res[, c("exposure", "n", "b", "se", "z", "p_wald", "p_lrt")]
  res$aic <- AIC(fit1aic)
  setDT(res)
  return(res)
}

#' ExtractFitsSplinesInteractions_Overall
#' Extract fits
#' @param fit0 The variable of interest
#' @param fit1 The variable of interest
#' @param fit1aic with a possibly more friendly family (possion instead of quasipoisson)
#' @param stack stack
#' @param i i
#' @param data data
#' @param form the formula
#' @param exposureValue a
#' @importFrom stringr str_replace_all str_extract
#' @importFrom stats model.frame coef vcov AIC
#' @importFrom splines ns
#' @import data.table
#' @export ExtractFitsSplinesInteractions_Overall
ExtractFitsSplinesInteractions_Overall <- function(fit0, fit1, fit1aic, stack, i, data, form, exposureValue=1){
  p_lrt <- RAWmisc::LRTest(fit0, fit1)
  res <- data.frame("b"=NA,
                    "se"=NA,
                    "z"=NA,
                    "p_wald" = NA)
  res$exposure <- sprintf("0 to %s, %s",exposureValue,stack$exposure[[i]])
  res$n <- sum(!is.na(fit1$fitted.values))
  res$p_lrt <- p_lrt
  res <- res[, c("exposure", "n", "b", "se", "z", "p_wald", "p_lrt")]
  res$aic <- AIC(fit1aic)
  setDT(res)
  return(res)
}

#' ExtractFitsSplines
#' Extract fits
#' @param fit0 The variable of interest
#' @param fit1 The variable of interest
#' @param fit1aic with a possibly more friendly family (possion instead of quasipoisson)
#' @param stack stack
#' @param i i
#' @param form the formula
#' @param data data
#' @param exposureValue exposureValue
#' @param runCombinations runCombinations
#' @importFrom stringr str_replace_all str_extract
#' @importFrom stats model.frame coef vcov AIC
#' @importFrom splines ns
#' @import data.table
#' @export ExtractFitsSplinesInteractions
ExtractFitsSplinesInteractions <- function(fit0, fit1, fit1aic, stack, i, data, form, exposureValue=1, runCombinations=TRUE){
  res <- list()
  res[[1]] <- ExtractFitsSplinesInteractions_Overall(
    fit0=fit0,
    fit1=fit1,
    fit1aic=fit1aic,
    stack=stack,
    i=i,
    data=data,
    form=form,
    exposureValue=exposureValue
  )
  if(runCombinations) if(!is.null(stack$nameInteractions[[i]])) if(!is.na(stack$nameInteractions[[i]])){
    values <- na.omit(unique(data[[stack$nameInteractions[[i]]]]))
    for(j in 1:length(values)){
      res[[j+1]] <- ExtractFitsSplines(
        fit0=fit0,
        fit1=fit1,
        fit1aic=fit1aic,
        stack=stack,
        i=i,
        data=data,
        form=form,
        exposureValue=exposureValue,
        nameInteractions=stack$nameInteractions[[i]],
        levelInteractions=values[j]
      )
      res[[j+1]][,exposure:=sprintf("COMBINATION AT %s=%s: %s",stack$nameInteractions[[i]],values[j],exposure)]
    }
  }
  res <- rbindlist(res)

  return(res)
}

#' CreateStackSkeleton
#' This creates the skeleton analysis stack
#' @param n The variable of interest
#' @export CreateStackSkeleton
CreateStackSkeleton <- function(n=1) {
  s <- data.frame(analysisID = UUID(n),stringsAsFactors = F)
  s$regressionType <- rep(NA, n)
  s$outcome <- NA
  s$exposure <- NA
  s$confounders <- NA
  s$nameBase <- NA
  s$nameInteractions <- NA
  s$data <- NA
  s$graphExposureScaleMultiply <- 1
  s$graphExposureScaleAdd <- 1
  s$graphReference <- 0
  s$graphExposureLocations <- NA
  s$graphExposureLocationsLabels <- NA
  s$graphFileName <- NA
  s$graphTitleMain <- NA
  s$graphTitleX <- NA

  return(s)
}

#' ValidateStack
#' This validates the skeleton analysis stack
#' @param stack stack
#' @param i The variable of interest
#' @importFrom stringr str_detect fixed
#' @export ValidateStack
ValidateStack <- function(stack,i=1) {
  graphExists <- FALSE
  for(j in CONFIG_STACK$GRAPH_VARS) if(!is.na(stack$graphFileName[[i]])) graphExists <- TRUE

  if(graphExists){
    for(j in CONFIG_STACK$GRAPH_VARS) if(is.null(stack[[j]][[i]])) return(FALSE)
    for(j in CONFIG_STACK$GRAPH_VARS) if(sum(is.na(stack[[j]][[i]]))>0) return(FALSE)
  }

  # if spline and interactions, cannot graph
  if(DetectSpline(stack$exposure[[i]]) & DetectInteraction(stack$exposure[[i]]) & graphExists){
    warning("Cannot have spline*interaction and graphing")
    return(FALSE)
  }

  # if spline and interactions, cannot graph
  if(sum(stringr::str_detect(stack$confounders[[i]],stringr::fixed(stack$exposure[[i]])))){
    warning("Cannot have exposure existing in confounders")
    return(FALSE)
  }

  return(TRUE)
}

#' CopyStack
#' This validates the skeleton analysis stack
#' @param stackFrom stackFrom
#' @param stackNew stackNew
#' @param i index stackFrom
#' @param j index stackNew
#' @export CopyStack
CopyStack <- function(stackFrom,stackNew,i=1,j=1) {
  for(n in names(stackFrom)){
    stackNew[[n]][j] <- stackFrom[[n]][i]
  }

  return(stackNew)
}

#' ExpandStack.int
#' This validates the skeleton analysis stack
#' @param stack stack
#' @param i The variable of interest
#' @param newAnalysisID If you want new analysis IDs generated (i.e. link broken between expanded stack and old stack)
#' @export ExpandStack.int
ExpandStack.int <- function(stack,i=1, newAnalysisID=FALSE) {
  stackNew <- RAWmisc::CreateStackSkeleton(n=length(stack$confounders[[i]])+1)


  for(j in 1:nrow(stackNew)){
    stackNew <- CopyStack(stackFrom=stack, stackNew=stackNew, i=i, j=j)
    allVars <- c(stack$exposure[[i]],stack$confounders[[i]])
    stackNew$exposure[[j]] <- allVars[j]
    stackNew$confounders[[j]] <- allVars[-j]

    for(k in CONFIG_STACK$GRAPH_VARS) stackNew[[k]][[j]] <- NA
  }

  if(newAnalysisID) stackNew$analysisID <- UUID()

  return(stackNew)
}

#' ExpandStack
#' This validates the skeleton analysis stack
#' @param stack stack
#' @param newAnalysisID If you want new analysis IDs generated (i.e. link broken between expanded stack and old stack)
#' @export ExpandStack
ExpandStack <- function(stack, newAnalysisID=FALSE) {
  stackFinal <- RAWmisc::CreateStackSkeleton(n=length(unlist(stack$confounders))+length(unlist(stack$exposure)))
  index <- 1
  for(j in 1:nrow(stack)){
    stackExpanded <- ExpandStack.int(stack,j)
    for(k in 1:nrow(stackExpanded)){
      stackFinal <- CopyStack(stackFrom=stackExpanded, stackNew = stackFinal,i=k,j=index)
      index <- index + 1
    }
  }

  return(stackFinal)
}

#' ProcessStack
#' This processes one value from the stack
#' @param stack The stack
#' @param i The i'th stack value
#' @param formatResults do you want the results formatted?
#' @importFrom stats glm binomial gaussian poisson coef as.formula
#' @importFrom MASS glm.nb
#' @importFrom stringr str_split str_replace str_detect fixed
#' @import ggplot2
#' @import data.table
#' @export ProcessStack
ProcessStack <- function(stack, i, formatResults=FALSE) {
  if (!stack$regressionType[[i]] %in% c("logistic", "linear","poisson","negbin")) {
    stop("Non-supported regression type")
  }
  if(!ValidateStack(stack,i)) stop("Stack not validated")

  regressionType <- NULL
  outcome <- NULL
  a_est <- NULL
  a_b <- NULL
  a_se <- NULL
  c_est <- NULL
  c_b <- NULL
  c_se <- NULL

  if (stack$regressionType[[i]] == "logistic") {
    aicFamily <- analysisFamily <- binomial()
    expResults <- TRUE
    graphTitleY <- "Odds ratio"
  } else if(stack$regressionType[[i]] == "linear"){
    aicFamily <- analysisFamily <- gaussian()
    expResults <- FALSE
    graphTitleY <- "Effect estimate"
  } else if(stack$regressionType[[i]] == "poisson"){
    aicFamily <- analysisFamily <- poisson()
    expResults <- TRUE
    graphTitleY <- "Incidence rate ratio"
  } else if(stack$regressionType[[i]] == "negbin"){
    aicFamily <- analysisFamily <- NULL
    expResults <- TRUE
    graphTitleY <- "Incidence rate ratio"
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

  for (j in c("crude0", "crude1", "aic_crude1", "adj0", "adj1", "aic_adj1")) {
    if (j %in% c("crude0", "crude1", "aic_crude1")) {
      dataUse <- dataCrude
    } else {
      dataUse <- dataAdj
    }
    if(j %in% c("aic_crude1","aic_adj1")){
      familyUse <- aicFamily
    } else {
      familyUse <- analysisFamily
    }
    formula_form <- stringr::str_replace(j,"aic_","")

    if(stack$regressionType[[i]] == "negbin"){
      fit[[j]] <- MASS::glm.nb(
        as.formula(get(sprintf("form_%s", formula_form))),
        data = dataUse
      )
    } else {
      fit[[j]] <- glm(
        as.formula(get(sprintf("form_%s", formula_form))),
        data = dataUse,
        family = familyUse
      )
    }
  }

  if(DetectInteraction(stack$exposure[[i]]) & DetectSpline(stack$exposure[[i]])){
    res_crude <- ExtractFitsSplinesInteractions(
      fit0 = fit[["crude0"]],
      fit1 = fit[["crude1"]],
      fit1aic = fit[["aic_crude1"]],
      stack = stack,
      i = i,
      data=dataCrude,
      form=form_crude1,
      runCombinations=F)
    res_adj <- ExtractFitsSplinesInteractions(
      fit0 = fit[["adj0"]],
      fit1 = fit[["adj1"]],
      fit1aic = fit[["aic_adj1"]],
      stack = stack,
      i = i,
      data=dataAdj,
      form=form_adj1)
  } else if(RAWmisc::DetectSpline(stack$exposure[[i]])){
    res_crude <- ExtractFitsSplines(
      fit0 = fit[["crude0"]],
      fit1 = fit[["crude1"]],
      fit1aic = fit[["aic_crude1"]],
      stack = stack,
      i = i,
      data=dataCrude,
      form=form_crude1)
    res_adj <- ExtractFitsSplines(
      fit0 = fit[["adj0"]],
      fit1 = fit[["adj1"]],
      fit1aic = fit[["aic_adj1"]],
      stack = stack,
      i = i,
      data=dataAdj,
      form=form_adj1)
  } else {
    res_crude <- ExtractFits(
      fit0 = fit[["crude0"]],
      fit1 = fit[["crude1"]],
      fit1aic = fit[["aic_crude1"]])
    res_adj <- ExtractFits(
      fit0 = fit[["adj0"]],
      fit1 = fit[["adj1"]],
      fit1aic = fit[["aic_adj1"]],
      nameBase = stack$nameBase[[i]],
      nameInteractions = stack$nameInteractions[[i]])
  }

  # wipe out res_crude if it is for interaction terms
  res_crude[IsInteraction(exposure),b:=NA]
  res_crude[IsInteraction(exposure),se:=NA]
  res_crude[IsInteraction(exposure),z:=NA]
  res_crude[IsInteraction(exposure),p_wald:=NA]
  res_crude[IsInteraction(exposure),p_lrt:=NA]
  res_crude[IsInteraction(exposure),aic:=NA]
  setnames(res_crude, c("exposure", "c_n", "c_b", "c_se", "c_z", "c_p_wald", "c_p_lrt","c_aic"))
  setnames(res_adj, c("exposure", "a_n", "a_b", "a_se", "a_z", "a_p_wald", "a_p_lrt","a_aic"))

  res <- merge(res_crude, res_adj, by = "exposure")
  if(sum(stringr::str_detect(res_adj$exposure,"COMBINATION"))){
    res2 <- merge(res_crude, res_adj[stringr::str_detect(exposure,"COMBINATION")], by = "exposure",all.y=T)
    res <- rbind(res,res2)
  }

  res[,analysisID := stack$analysisID[[i]]]
  res[, regressionType := stack$regressionType[[i]]]
  res[, outcome := stack$outcome[[i]]]

  # graphing results
  if(!is.na(stack$graphFileName[[i]])){
    #### TO GRAPH
    toGraph <- vector("list",length=length(stack$graphExposureLocations[[i]]))
    for(j in 1:length(toGraph)){
      ev <- stack$graphExposureLocations[[i]][j]
      if(RAWmisc::DetectSpline(stack$exposure[[i]])){
        temp <- ExtractFitsSplines(
          fit0 = fit[["adj0"]],
          fit1 = fit[["adj1"]],
          fit1aic = fit[["aic_adj1"]],
          stack = stack,
          i = i,
          data=dataAdj,
          form=form_adj1,
          exposureValue = ev)
      } else {
        temp <- ExtractFits(
          fit0 = fit[["adj0"]],
          fit1 = fit[["adj1"]],
          fit1aic = fit[["aic_adj1"]],
          exposureValue = ev)
      }
      temp[,exposureValue:=ev]
      toGraph[[j]] <- temp[stringr::str_detect(exposure,stringr::fixed(stack$exposure[[i]]))]
    }
    toGraph <- rbindlist(toGraph)
    ref <- toGraph[1]
    ref$b <- 0
    ref$se <- 0
    ref$exposureValue <- stack$graphReference[[i]]

    toGraph <- rbindlist(list(toGraph,ref))

    #### TO GRAPH LABELS
    toGraphLabels <- NULL
    if(!is.na(stack$graphExposureLocationsLabels[[i]][1])){
      toGraphLabels <- vector("list",length=length(stack$graphExposureLocationsLabels[[i]]))
      for(j in 1:length(toGraphLabels)){
        ev <- stack$graphExposureLocationsLabels[[i]][j]
        if(RAWmisc::DetectSpline(stack$exposure[[i]])){
          temp <- ExtractFitsSplines(
            fit0 = fit[["adj0"]],
            fit1 = fit[["adj1"]],
            fit1aic = fit[["aic_adj1"]],
            stack = stack,
            i = i,
            data=dataAdj,
            form=form_adj1,
            exposureValue = ev)
        } else {
          temp <- ExtractFits(
            fit0 = fit[["adj0"]],
            fit1 = fit[["adj1"]],
            fit1aic = fit[["aic_adj1"]],
            exposureValue = ev)
        }
        temp[,exposureValue:=ev]
        toGraphLabels[[j]] <- temp[stringr::str_detect(exposure,stringr::fixed(stack$exposure[[i]]))]
      }
      toGraphLabels <- rbindlist(toGraphLabels)
      ref <- toGraphLabels[1]
      ref$b <- 0
      ref$se <- 0
      ref$exposureValue <- stack$graphReference[[i]]

      toGraphLabels <- rbindlist(list(toGraphLabels,ref))
      toGraphLabels[, est := RAWmisc::FormatEstCIFromEstSE(beta = b, se = se, exp = expResults)]
      #fixing ref to not have CIs
      toGraphLabels[exposureValue==stack$graphReference[[i]],
              est:=sprintf("%s (reference)",ifelse(expResults,exp(stack$graphReference[[i]]),stack$graphReference[[i]]))]
    }
    #### END

    if(expResults){
      toGraph[,l95:=exp(b-1.96*se)]
      toGraph[,u95:=exp(b+1.96*se)]
      toGraph[,b:=exp(b)]
      if(!is.null(toGraphLabels)) toGraphLabels[,b:=exp(b)]
    } else {
      toGraph[,l95:=b-1.96*se]
      toGraph[,u95:=b+1.96*se]
    }
    toGraph[,exposureValueScaled:=exposureValue*stack$graphExposureScaleMultiply[[i]]+stack$graphExposureScaleAdd[[i]]]
    if(!is.null(toGraphLabels)) toGraphLabels[,exposureValueScaled:=exposureValue*stack$graphExposureScaleMultiply[[i]]+stack$graphExposureScaleAdd[[i]]]

    xMin <- min(toGraph$exposureValueScaled)
    xMax <- max(toGraph$exposureValueScaled)
    dif <- xMax-xMin
    q <- ggplot(data=toGraph,mapping=aes(x=exposureValueScaled,y=b))
    if(expResults){
      q <- q + geom_hline(yintercept = 1,col="black",lty=2)
    } else {
      q <- q + geom_hline(yintercept = 0,col="black",lty=2)
    }
    q <- q + geom_ribbon(alpha=0.4,mapping=aes(ymin=l95,ymax=u95))
    q <- q + geom_line()
    if(!is.null(toGraphLabels)){
      q <- q + geom_point(data=toGraphLabels)
      q <- q + geom_text(data=toGraphLabels,
                         mapping=aes(label=est),
                         alpha=1,
                         angle=90,
                         hjust=-0.1,
                         size=(5/14) * 16)
    }
    q <- q + scale_x_continuous(stack$graphTitleX[[i]])
    q <- q + scale_y_continuous(graphTitleY)
    q <- q + expand_limits(x=c(xMin-dif*0.10,xMax+dif*0.10))
    q <- q + labs(title=stack$graphTitleMain[[i]])
    q <- q + theme_grey(base_size = 16)
    saveA4(q,stack$graphFileName[[i]])
  }




  if (formatResults) {
    res[, a_est := RAWmisc::FormatEstCIFromEstSE(beta = a_b, se = a_se, exp = expResults)]
    res[, c_est := RAWmisc::FormatEstCIFromEstSE(beta = c_b, se = c_se, exp = expResults)]

    res <- res[res$exposure != "(Intercept)", c(
      "analysisID",
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
      "analysisID",
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
      "analysisID",
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
      "analysisID",
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
