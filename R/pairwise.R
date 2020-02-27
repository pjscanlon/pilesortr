pairwise <- function(similarity){
  output <- data.frame()
  for (r in 1:length(similarity)){
    output <- rbind(output, similarity[[r]][upper.tri(similarity[[r]])])
  }
  output
}
