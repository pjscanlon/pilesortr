#' Comparative Analysis of Pile Sorts
#' @description Provides subgroup analysis of pile sorts using either/or a quadratic assignment proceedure or a modification that Borgatti (2002) suggested was more appropriate for the analysis of subsamples of pile sorts. Uses \code{\link{QAP_analysis}} and \code{\link{borgatti_test}}, respectively.
#' @param data A list of similarity matrices (such as returned by \code{\link{load_piles}}), with list length equal to the total number of pile sorts in the sample.
#' @param group1 A matrix.  The aggregate similarity matrix (such as returned by \code{\link{load_piles}}) for the first comparison subgroup.
#' @param group2 A matrix.  The aggregate similarity matrix (such as returned by \code{\link{load_piles}}) for the second comparison subgroup.
#' @param plot Logical. Prints the analytic plots related to the subgroup analysis methods.  Defaults to \code{FALSE}.
#' @param reps Numeric. Number of correlations between the sample data and the simulated matrices for the Borgatti Test.  Default is 1000.
#' @return A list of two objects \enumerate{
#'     \item An object of class \code{qaptest}.  See \code{\link[sna]{qaptest}} for more information.
#'     \item A numeric value representing the proportion of the correlations that were higher than the direct correlations between \code{group1} and \code{group2}.
#'  }
#' @export

compare_piles <- function(data,group1,group2,reps=1000,qap=TRUE,borgatti=TRUE,plot=FALSE){#group1 and 2 are agg similarity#
  if(qap==TRUE){
    QAP <- QAP_analysis(group1,group2,plot = plot)
  }
  if(qap==FALSE){
    QAP <- NULL
  }
  if(borgatti==TRUE){
    Borgatti <- borgatti_test(data,group1,group2,reps,plot = plot)
    Dissimilarity <- 1-cor(as.vector(group1), as.vector(group2))
  }
  if(borgatti==FALSE){
    Borgatti <- NULL
    Dissimilarity <- NULL
  }
  out=list(QAP = QAP, Borgatti = Borgatti, Dissimilarity = Dissimilarity)
  out
}
