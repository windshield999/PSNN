#'Check the balance of the adjusted covariates by histogram, barplot and
#'qqplot
#'@export


CheckBalance <- function(data, ps, trt,ind){
  #1-1 match
  datovlp <- data[ind,]
  trtovlp <- trt[ind]
  names(trtovlp) <- 1:length(trtovlp)
  psovlp <-ps[ind]
  names(psovlp)<-1:length(psovlp)

  fm1 <- optmatch::pairmatch(x =psovlp, z=trtovlp, method = "mahalanobis", controls = 1, data = trtovlp)
  matcheddata <- datovlp[optmatch::matched(fm1),]
  return(matcheddata)



}
