#' Cluster Analysis for Pile Sorts
#' @description A wrapper for \code{hclus} and \code{ggdendrogram} that takes an aggregate dissimilarity matrix (such as that produced by \code{compile_piles}) and returns a hierarchical cluster dendrogram
#' @param aggregate_dissimilairity An n by n matrix, where n is the number of items in the pile sort, that provides the distance between the items.
#' @param method A string.  See \code{hclust} for options.  Default to "mcquitty"
#' @param plot Logical. Prints the dendrogram. Defaults to TRUE.
#' @param plot_title A string. The title of the MDS plot.  Defaults to NULL.  If NULL, plot is untitled.
#' @return A list of two: 1) and hclust object that describes the tree produced by the clustering process.  See \code{hclust} for details. 2) A ggplot object depicting the resulting dendrogram from the clustering.
#' @export
cluster_piles <- function(aggregate_dissimilarity,method="mcquitty",plot=TRUE,plot_title=NULL, reverse=FALSE){
  hc <- as.dendrogram(hclust(dist(t(aggregate_dissimilarity)),method = method))
  if(reverse==TRUE){
    hc <- rev(hc)
  }
  cp <- ggdendrogram(hc,rotate = T)+ggtitle(plot_title) + theme(plot.title = element_text(hjust = 0.5))
  if(plot==TRUE){
    print(cp)
  }
  return(list(hc=hc,plot=cp))
}
