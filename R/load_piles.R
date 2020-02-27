load_piles <- function(dataset, 
                      cardnames, 
                      variable=NULL, 
                      subgroup=NULL, 
                      comparison=c("equal", "not equal", "less", "greater"), 
                      include.equal=FALSE){
  #first establish "comp" variable based on user input of "comparison" and "include.equal"#
  if(include.equal==FALSE){
    comp <- ifelse(comparison=="equal", "eq", 
                   ifelse(comparison=="not equal", "ne", 
                          ifelse(comparison=="less", "l", 
                                 ifelse(comparison=="greater", "g", "eq"))))
  }
  else if (include.equal==TRUE){
    comp <- ifelse(comparison=="equal", "eq", 
                   ifelse(comparison=="not equal", "ne", 
                          ifelse(comparison=="less", "le",
                                 ifelse(comparison=="greater", "ge", "eq"))))
    
  }
  
  #Next, the function compile_piles is used to read pile sort data into R and transform it into the matrices#
  compile_piles(list_piles(pull_list(dataset = dataset, variable = variable, subgroup = subgroup, comp = comp)), 
                cardnames = cardnames)
}