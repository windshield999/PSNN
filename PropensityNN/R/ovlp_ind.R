#'Examine if there is an overlapping for each observation.
#'@param trt Treatment assignment
#'@param lps Logit propensity score
#'@param caliper Parameter defining nonoverlapping
#'@export


ovlp_ind <- function(trt, lps, caliper = 0.1)
{
  nt <- sum(trt == 1); nc <- sum(trt == 0)
  SDpool <- sqrt( ( (nt - 1)*var(lps[trt == 1]) + (nc - 1)*var(lps[trt == 0]) ) /
                    (nt + nc - 2) )
  ### Get abs(distance) for each treat/control pairing
  diffs <- optmatch::match_on(trt ~ lps, method = "euclidean")
  smds <- diffs/SDpool # standardize differences by dividing by pooled SD
  fun <- function(vec) {min(vec) <= caliper}
  trtOvlp <- apply(smds, 1, fun)   # TRUEs are overlapping
  ctrlOvlp <- apply(smds, 2, fun)  # FALSEs are not
  drop1 <- which(trt==1)[!trtOvlp]
  drop0 <- which(trt==0)[!ctrlOvlp]
  ind <- !logical(length(lps))
  ind[c(drop1, drop0)] <- FALSE

  #if more than a half observations are discarded, the stop and give error message.
  if (2*sum(ind) < length(trt))
    stop ("Not enough overlap. Try choosing a larger weight decay value.")

  #Plot the new histograms for corrected data
  ovlp(trt[ind], lps[ind])
  ind
}
