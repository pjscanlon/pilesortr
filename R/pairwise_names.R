pairwise_names <- function(cardnames){
  #Create row vector
  k <- c(1:length(cardnames))
  vec <- vector()
  for(i in k){
    vec <- c(vec,rep(i,times=i-1))
  }

  #Create column vector
  j <- c(1:(length(k)-1))
  vec2 <- vector()
  for(i in j){
    vec2 <- c(vec2,c(1:i))
  }
  list(vec,vec2)

  #Create name postion matrix
  name_mat <- matrix(data = c(vec,vec2), byrow = TRUE, nrow = 2)

  #Use for loop to create final column name vector
  l <- (length(cardnames)*(length(cardnames)-1))/2
  names <- vector(length = l)
  for(i in 1:l){
    names[i]=paste(cardnames[name_mat[1,i]], cardnames[name_mat[2,i]],sep = "/")
  }

  #output is names vector
  names
}
