pairwise_matrix <- function(similarity,cardnames){ #similarity is a list of similarity matrices (all_similarity from Compile output)
  names <- ps_PairwiseNames(cardnames)

  output <- ps_Pairwise(similarity)

  colnames(output) <- names

  output
}
