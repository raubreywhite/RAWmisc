#' SummarizeBinary
#' This summarizes binary variables
#' @param var The variable of interest
#' @import data.table
#' @export SummarizeBinary
SummarizeBinary <- function(var) {
  # variables used in data.table functions in this function
  . <- NULL
  n <- NULL
  label <- NULL
  percent <- NULL
  # end

  x <- data.table(x = var)
  x <- x[, .(n = .N), by = .(x)]
  skeleton <- data.table(x = c(1, 0, NA))
  skeleton <- merge(skeleton, x, by = "x", all.x = T)
  skeleton[is.na(n), n := 0]
  skeleton[is.na(x), x := -1]
  setorder(skeleton, -x)
  skeleton[, label := c("True", "False", "Missing")]
  skeleton[x != -1, percent := sum(n)]
  skeleton[, percent := sprintf("%s%%", RAWmisc::Format(n / percent * 100, digits = 1))]
  skeleton[x == -1, percent := "-"]
  skeleton[, x := NULL]
  setcolorder(skeleton, c("label", "n", "percent"))

  return(skeleton)
}

#' SummarizeContinuous
#' This summarizes continuous variables
#' @param var The variable of interest
#' @importFrom stats median quantile
#' @import data.table
#' @export SummarizeContinuous
SummarizeContinuous <- function(var) {
  # variables used in data.table functions in this function
  . <- NULL
  n <- NULL
  med <- NULL
  median_iqr <- NULL
  p25 <- NULL
  p75 <- NULL
  label <- NULL
  # end

  x <- data.table(x = var)
  x[, label := "Missing"]
  x[!is.na(x), label := "Median (IQR)"]
  x <- x[, .(
    n = .N,
    med = median(x, na.rm = T),
    p25 = quantile(x, probs = 0.25, na.rm = T),
    p75 = quantile(x, probs = 0.75, na.rm = T)
  ), by = label]
  x[, median_iqr := sprintf(
    "%s (%s-%s)",
    RAWmisc::Format(med),
    RAWmisc::Format(p25),
    RAWmisc::Format(p75)
  )]
  x <- x[, c("label", "n", "median_iqr")]
  skeleton <- data.table(label = c("Median (IQR)", "Missing"))
  skeleton <- merge(skeleton, x, by = "label", all.x = T)
  skeleton[is.na(n), n := 0]
  skeleton[label == "Missing", median_iqr := "-"]

  return(skeleton)
}

#' SummarizeCategory
#' This summarizes categorical variables
#' @param var The variable of interest
#' @import data.table
#' @export SummarizeCategory
SummarizeCategory <- function(var) {
  # variables used in data.table functions in this function
  . <- NULL
  n <- NULL
  percent <- NULL
  # end

  x <- data.table(x = var)
  x <- x[, .(n = .N), by = .(x)]
  setorder(x, -x)
  skeleton <- data.table(x = unique(c(NA, as.character(unique(var)))))
  skeleton <- merge(skeleton, x, by = "x", all.x = T)
  skeleton[, x := factor(x, levels = levels(var))]
  setorder(skeleton, -x)
  skeleton[, x := as.character(x)]
  skeleton[is.na(n), n := 0]
  skeleton[is.na(x), x := "Missing"]
  skeleton <- skeleton[.N:1]

  skeleton[x != "Missing", percent := sum(n)]
  skeleton[, percent := sprintf("%s%%", RAWmisc::Format(n / percent * 100, digits = 1))]
  skeleton[x == "Missing", percent := "-"]

  return(skeleton)
}

#' AddEmptyRowsAtTop
#' This adds empty rows to the top of a data.frame
#' @param d The data.frame of interest
#' @param numRows The number of rows to add on the top
#' @import data.table
#' @export AddEmptyRowsAtTop
AddEmptyRowsAtTop <- function(d, numRows) {
  retval <- data.table(rbind(d[1:numRows], d))
  for (i in names(retval)) {
    retval[, (i) := as.character(get(i))]
    for (j in 1:numRows) retval[j, (i) := ""]
    retval[numRows, (i) := i]
  }
  return(retval)
}

#' SummarizeDispatchOneVar
#' Summarizes one variable (mostly for internal use)
#' @param var The variable of interest
#' @param labelLeft Adding a label to the left of the returned data.frame
#' @param labelTop Adding a label to the top of the returned data.frame
#' @import data.table
#' @export SummarizeDispatchOneVar
SummarizeDispatchOneVar <- function(var, labelLeft = NULL, labelTop = NULL) {
  if (sum(!unique(var) %in% c(1, 0, NA)) == 0) {
    retval <- SummarizeBinary(var)
  } else if (is.factor(var)) {
    retval <- SummarizeCategory(var)
  } else {
    retval <- SummarizeContinuous(var)
  }

  if (!is.null(labelLeft)) {
    retval <- cbind(rep(labelLeft, nrow(retval)), retval)
  }

  if (!is.null(labelTop)) {
    retval <- AddEmptyRowsAtTop(retval, 3)
    retval[2, 3] <- labelTop
  } else {
    retval <- AddEmptyRowsAtTop(retval, 2)
  }
  return(retval)
}

#' SummarizeDispatch
#' Summarizes one variable, with the option of stratifying by a 0/1 variable
#' @param var The variable of interest
#' @param by A 0/1 variable to be stratified by (optional)
#' @param labelLeft Adding a label to the left of the returned data.frame
#' @param labelTop Adding a label to the top of the returned data.frame (only to be used if by=NULL)
#' @param labelTop0 Adding a label to the top left of the returned data.frame (only to be used if by!=NULL)
#' @param labelTop1 Adding a label to the top right of the returned data.frame (only to be used if by!=NULL)
#' @import data.table
#' @export SummarizeDispatch
SummarizeDispatch <- function(var, by = NULL, labelLeft = NULL, labelTop = NULL, labelTop0 = NULL, labelTop1 = NULL) {
  if (!is.null(by)) {
    keep <- !is.na(by)
    var <- var[keep]
    by <- by[keep]
    if (length(unique(by)) != 2) {
      stop("There can only be 2 categories in the 'by' variable")
    }
    retval0 <- SummarizeDispatchOneVar(var[by == 0], labelLeft = labelLeft, labelTop = labelTop0)
    retval1 <- SummarizeDispatchOneVar(var[by == 1], labelLeft = labelLeft, labelTop = labelTop1)
    return(cbind(retval0, X = "", X = "", retval1))
  } else {
    return(SummarizeDispatchOneVar(var, labelLeft = labelLeft, labelTop = labelTop))
  }
}
