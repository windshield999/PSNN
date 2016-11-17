sc.nn <- function(dat)
{
  # ::: This function converts factor variable to numeric variables,
  # ::: then scales the values to range [0,1]. The purpose of the
  # ::: function is to process the data before running neural networks.

  sc.01 <- function(vec) {(vec - min(vec, na.rm = TRUE)) / diff(range(vec, na.rm = TRUE))}
  if (is.vector(dat)){return(sc.01(dat))}
  print("123")

  if(is.data.frame(dat)){
    fact.levels <- as.numeric(sapply(dat, function(x) length(levels(x))))
  } else if(is.matrix(dat)){
    fact.levels <- as.numeric(apply(dat, 2,function(x) length(levels(x))))
  }






  if(is.data.frame(dat[,which(fact.levels == 0)])){
    print(1)
    dat[,which(fact.levels == 0)] <- sapply(dat[,which(fact.levels == 0)], sc.01)
  }else if(is.matrix(dat[,which(fact.levels == 0)])){
    print("1234")
    dat[,which(fact.levels == 0)] <- apply(X = dat[,which(fact.levels == 0)],MARGIN = 2,FUN =  sc.01)
  }

  dat
}

