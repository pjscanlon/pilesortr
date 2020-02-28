#' Multidimensional Scaling for Pile Sorts
#' @description Takes an aggregate dissimilarity matrix from across a number of pile sorts (such as what is returned by \code{compile_piles}) and returns a multi-dimensional scaling plot.
#' @param aggregate_dissimilairity An n by n matrix, where n is the number of items in the pile sort, that provides the distance between the items.
#' @param plot Logical. Prints the MDS plot. Defaults to TRUE.
#' @param plot_title A string. The title of the MDS plot.  Defaults to NULL.  If NULL, plot is untitled.
#' @param plot_type A string. "Text" plots the card names; "Point" plots points; "Both" plots both text and points.
#' @return A list of two: 1) A matrix with k=\code{dimensions} columns and rows of n=number of items in pile sort. 2) A ggplot object depicting the MDS
#' @export
MDS_piles <- function(aggregate_dissimilarity, plot = TRUE, plot_title = NULL, dimensions = 2, plot_type = "None",
                      reverse_chart = FALSE){
  require(ggplot2)
  mds <- cmdscale(aggregate_dissimilarity, k = dimensions)
  if(reverse_chart==TRUE){mds = mds* -1}
  mds_plot <- ggplot(as.data.frame(mds), aes(V1, -V2, label = rownames(mds))) +
    theme_minimal() + xlab('') + ylab('') +
    scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)+
    ggtitle(plot_title) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  if(plot_type=="Text"){
    mds_plot <- mds_plot + geom_text(check_overlap = TRUE)
  }
  if(plot_type=="Point"){
    mds_plot <- mds_plot + geom_point()
  }
  if(plot_type=="Both"){
    mds_plot <- mds_plot + geom_point() + geom_text(check_overlap = TRUE)
  }
  if(plot==TRUE){
    print(mds_plot)
  }
  return(list(mds <- mds,plot <-mds_plot))
}
