pull_list = function(dataset, variable=NULL, subgroup=NULL, comp=c("eq","ne","g", "ge", "l", "le")) {
  arg1 <- quote(list(x = dataset, y = NULL, z = NULL))
  arg2 <- quote(list(x = dataset, y = variable, z = subgroup))
  operation1 <- quote(x)
  operation2 <- quote(x[x[,which(colnames(x)==y)]==z,])
  operation3 <- quote(x[x[,which(colnames(x)==y)]!=z,])
  operation4 <- quote(x[x[,which(colnames(x)==y)]>z,])
  operation5 <- quote(x[x[,which(colnames(x)==y)]>=z,])
  operation6 <- quote(x[x[,which(colnames(x)==y)]<z,])
  operation7 <- quote(x[x[,which(colnames(x)==y)]<=z,])
  
  if(is.null(variable)){
    as.character(eval(operation1, eval(arg1))$File_Location)
  }
  else if (is.null(subgroup)){
    stop("Error: Missing subgroup")
  }
  else if(comp=="eq"){
    as.character(eval(operation2, eval(arg2))$File_Location)
  }
  else if(comp=="ne"){
    as.character(eval(operation3, eval(arg2))$File_Location)
  }
  else if (comp=="g"){
    as.character(eval(operation4, eval(arg2))$File_Location)
  }
  else if (comp=="ge"){
    as.character(eval(operation5, eval(arg2))$File_Location)
  }
  else if (comp=="l"){
    as.character(eval(operation6, eval(arg2))$File_Location)
  }
  else if (comp=="le"){
    as.character(eval(operation7, eval(arg2))$File_Location)
  }
}