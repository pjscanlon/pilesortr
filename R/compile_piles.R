#' Creation of Simiarility and Dissimilarity Matrices for Pile Sorts
#'@description A function that takes a list of dataframes that each contain the results of a single pile sort and creates individual and aggreage similarity matrices.
#'@param card_data A list of dataframes, such as that produced by \code{list_piles}.
#'@param cardnames A vector of strings that contains the names of each individual item (or card) in the pile sort.
#'@return A list that include the individual similarity matrices and the aggregate similarity and dissimilarity matrices
#'@export
compile_piles=function(card_data,cardnames = cardnames){
  #each pile sort dataframe is transformed into a similarity matrix#
  all_similarity=list()
  card_names_m=list(cardnames,cardnames)
  for(l in 1:length(card_data)){
    dat=card_data[[l]]
    
    for (i in 1:length(dat)){
      temp=dat[,i]
      temp[temp==1]=i
      dat[,i]=temp
    }
    groups=rowSums(dat,na.rm = T)
    sim=c()
    
    for (j in 1:length(groups)){
      sim_t=match(groups,groups[j],0)
      sim=c(sim,sim_t)
    }
    
    similarity=matrix(sim,nrow = length(groups),ncol = length(groups),byrow = T,dimnames = card_names_m)
    
    #this will combine all output similarity matrices into a list of similarly matrices#
    all_similarity[[l]]=similarity
  }
  
  #following the creation of the individual simiarlity matrices, 
  #they are aggregated into both aggregate similarity and dissimilarity matrices#
  agg_similarity=Reduce('+',all_similarity)
  agg_dissimilarity=(length(card_data)-agg_similarity)#/length(card_data)
  
  #The output is a list of three items#
  #The first is a list of the individual similarity matrices; 
  #The second is the aggregate similarity matrix#
  #The third is the the aggregate dissimilarity matrix#
  return(list(all_similarity=all_similarity,
              agg_similarity=agg_similarity,
              agg_dissimilarity=agg_dissimilarity))
}
