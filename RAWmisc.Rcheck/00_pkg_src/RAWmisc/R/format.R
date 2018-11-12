#' Calculates pvalues
#' @param beta a
#' @param se a
#' @export CalcPValue
CalcPValue <- function(beta, se) {
  2 * (1 - pnorm(abs(beta / se)))
}

#' Allows for recoding of variables
#' @param x a
#' @param digits a
#' @export Format
Format <- function(x, digits = 2) {
  formatC(x, digits = digits, format = "f")
}

#' Allows for recoding of variables
#' @param beta a
#' @param se a
#' @param digits a
#' @param exp a
#' @export FormatEstCIFromEstSE.int
FormatEstCIFromEstSE.int <- function(beta, se, digits = 2, exp = TRUE) {
  l95 <- beta - 1.96 * se
  u95 <- beta + 1.96 * se
  if (exp) {
    beta <- exp(beta)
    l95 <- exp(l95)
    u95 <- exp(u95)
  }
  retval <- sprintf(
    "%s (%s, %s)",
    RAWmisc::Format(beta, digits),
    RAWmisc::Format(l95, digits),
    RAWmisc::Format(u95, digits)
  )
}

#' Allows for recoding of variables
#' @param beta a
#' @param se a
#' @param digits a
#' @param exp a
#' @export FormatEstCIFromEstSE
FormatEstCIFromEstSE <- Vectorize(FormatEstCIFromEstSE.int)

#' Allows for recoding of variables
#' @param beta a
#' @param va a
#' @param nameBase a
#' @param nameInteractions a
#' @importFrom stats pnorm
#' @export ExtractInteractedEffectEstimates
ExtractInteractedEffectEstimates <- function(beta, va, nameBase, nameInteractions) {
  if (is.null(ncol(beta))) {
    beta <- as.data.frame(t(beta))
  }
  lincom <- matrix(0, ncol = ncol(beta), nrow = (1 + length(nameInteractions)))
  lincom[, which(names(beta) == nameBase)] <- 1
  for (i in 1:length(nameInteractions)) {
    lincom[i + 1, which(names(beta) == nameInteractions[i])] <- 1
  }
  beta <- c(lincom %*% t(beta))
  se <- rep(0, length(beta))

  for (i in 1:length(beta)) {
    vars <- diag(va)[which(lincom[i, ] == TRUE)]
    id <- which(lincom[i, ] == TRUE)
    covar <- va[id[1], id[2]]
    if (i == 1) {
      se[i] <- sqrt(vars)
    } else {
      se[i] <- sqrt(sum(vars) + 2 * covar)
    }
  }
  p <- RAWmisc::CalcPValue(beta = beta, se = se)
  return(list("beta" = beta, "se" = se, "p" = p))
}
