#' Takes a list of .csv file pathways, reads each .csv into a dataframe, and combines these into a list
#' @param filelist A list or vector of strings.  Each individual item in the list must be a string representing the filepath of a .csv file that contains the results of individual pile sorts.
#' @return A list of dataframes of length equivalent to the length of \code{filelist}
#' @export
list_piles <- function(filelist){
  card_data <- lapply(filelist, read.csv, header=F)
  return(card_data)
}
