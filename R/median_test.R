#' Monte-Carlo test of differences between medians
#'
#' This is a test we developed with DIDE's MRes.
#'
#' @param x A vector of numeric values
#' @param fac A factor defining 2 groups
#' @param n The number of observations
#'
#' @export
#'
#' @examples
#'
#' ## CREATE DATA ##
#' set.seed(1)
#' titer <- c(rexp(100, rate = 0.002), rexp(20, rate = 0.0005), # control
#'         rexp(55, rate = 0.006), rexp(15, rate = 0.0005)) # treatment
#'
#' type <- factor(rep(c("control","treatment"), c(120,70)))
#'
#' ## make a data frame
#' VL <- data.frame(titer = titer, type = type)

# Making extra comments here
# Even more comments
# Hello

median_test <- function(x, fac, n = 999, ...){
  ## check dependencies
  if (!require(ade4)) stop("ade4 is not installed")

  ## function to get the statistic
  f1 <- function(x, fac) {
    medians <- tapply(x, fac, median)
    return(medians[1] - medians[2])
  }

  ## get original statistic
  stat.ori <- f1(x,fac)

  ## get permuted values
  stat.perm <- replicate(n, f1(x, sample(fac)))

  ## return output
  out <- ade4::as.randtest(sim = stat.perm, obs = stat.ori, ...)
  return(out)
}
