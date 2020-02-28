#' Quadratic Assignment Proceedure (QAP) for Pile Sorts
#' @description A wrapper for \code{\link[sna]{qaptest}} using pilesortr objects.  Takes the aggregate similarity matrices for two subgroup's pile sorts and combines them in order to run a QAP.
#' @param group1 A matrix.  The aggregate similarity matrix (such as returned by \code{\link{compile_piles}}) for the first comparison subgroup.
#' @param group2 A matrix.  The aggregate similarity matrix (such as returned by \code{\link{compile_piles}}) for the second comparison subgroup.
#' @param plot Prints a plot of the distribution of the test statistic.  Defaults to \code{FALSE}.
#' @return An object of class \code{qaptest}.  See \code{\link[sna]{qaptest}} for more information.  Object contains \itemize{
#'   \item \code{testval}  The observed value of the test statistic.
#'   \item \code{dist}  A vector containing the Monte Carlo draws.
#'   \item \code{pgreq}  The proportion of draws which were greater than or equal to the observed value.
#'   \item \code{pleeq}  The proportion of draws which were less than or equal to the observed value.
#' }
#' @export

QAP_analysis <- function(group1, group2, plot=FALSE){
  library(sna)
  combo_groups <- list(max (group1)-group1, max(group2) - group2) #list of groups' dissimilarity matrices#
  qap <- qaptest(combo_groups, gcor, g1=1, g2=2)
  if (plot==TRUE){
    print(plot(qap))
  }
  return(qap <- qap)
}
