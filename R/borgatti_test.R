#' borgatti_test
#' @description Runs the alternative to Quadratic Assignment Proceedure (QAP) that Borgatti 2002 developed and recommended for the comparison of pile sorts across subgroups.
#' @param data A list of similarity matrices (such as returned by \code{\link{compile_piles}}), with list length equal to the total number of pile sorts in the sample.
#' @param group1 A matrix.  The aggregate similarity matrix (such as returned by \code{\link{compile_piles}}) for the first comparison subgroup.
#' @param group2 A matrix.  The aggregate similarity matrix (such as returned by \code{\link{compile_piles}}) for the second comparison subgroup.
#' @param plot Logical. Prints the density plot of the correlations.  Defaults to \code{FALSE}.
#' @param reps Numeric. Number of correlations between the sample data and the simulated matrices.  Default is 1000.
#' @return A numeric value representing the proportion of the correlations that were lower than the direct correlations between \code{group1} and \code{group2}.
#' @references Borgatti,Stephen P. 2002. "A Statistical Method for Comparing Aggregate Data Across A Priori Groups." \emph{Field Methods} 14(1): 88-107.
#' @export
borgatti_test <- function(data, group1, group2, plot = FALSE, reps = 1000){
  correlations <- vector()
  group1 <- as.vector(group1) #flattened aggregate similarity matrix for group 1#
  group2 <- as.vector(group2) #flattened aggregatesimiilarity matrix for group 2#
  for (i in 1:reps){
    r1 <- sample(1:length(data), sample(1:(length(data)-1), 1, replace = F))
    r2 <- setdiff(1:length(data), r1)
    agg_sim_r1 <- as.vector(Reduce('+',data[r1]))
    agg_sim_r2 <- as.vector(Reduce('+',data[r2]))
    correlations <- c(correlations, cor.test(agg_sim_r1, agg_sim_r2)$estimate)
  }
  direct_correlation <- cor.test(group1, group2)$estimate
  borgatti <- sum(correlations<=direct_correlation, na.rm = T)/length(correlations)
  if(plot==TRUE){
    print(plot(density(correlations)))
    return(borgatti=borgatti)
  }
  else{return(borgatti=borgatti)
  }
}
