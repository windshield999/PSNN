#'bb
#'bb
#'@export
smd_vr_DF <- function(covars, dat, trt, wts = rep(1, length(trt)), plot = FALSE)
{
  ### This function may be used to apply smd_vr over an entire data frame "dat"
  ### on selected covariates "covars" that can be found in the data frame "dat".
  ### We assume there are no factors in the data frame. The function also plots
  ### if requested.

  output <- matrix(0, length(covars), 2)
  colnames(output) <- c("St Mean Diff", "Var Ratio")

  ### Run smd_vr for each covariate with a for loop
  for(i in 1:length(covars)) {
    output[i,] <- smd_vr(trt = trt, covar = dat[,covars[i]], wts = wts)
  }
  rownames(output) <- covars

  ### If plot == TRUE make a plot
  if(plot) {
    plot(output, pch = 19, xlim = c(-1, 1), ylim = c(0, 3.5))
    abline(v = c(-.1, .1), lwd = 2, lty = 2)
    abline(h = c(4/5, 5/4), lwd = 2, lty = 2)
    lbls <- rownames(output)
    for (i in 1:length(lbls)) {
      if(output[i,2] < 5/4 & output[i,2] > .8 & output[i,1] < .1 & output[i,1] > -.1) {
        lbls[i] <- ""
      }
    }
    text(output, labels = lbls, adj = c(0, -.3))
  }
  output
}
smd_vr <- function(trt, covar, wts = rep(1, lenght(cov)))
{
  ### This function calculates the standardized mean difference and variance
  ### ratios for the variable "cov", across treatment groups "trt".
  ### "trt" is the treatment group indicator, cov is the covariate of interest.

  ### Which rows were treated?
  trt_ind <- trt == 1
  ctrl_ind <- trt == 0

  ### Sample sizes of treated and comparison groups
  nt <- sum(trt == 1)
  nc <- sum(trt == 0)

  ### Blank vector for storing output
  out <- NULL

  ### Standardized mean differences
  out[1] <- (mean(covar[trt_ind]) - mean(covar[ctrl_ind])) /
    sqrt( ( (nt - 1)*var(covar[trt_ind]) + (nc - 1)*var(covar[ctrl_ind]) ) /
            (nt + nc - 2) )

  ### Variance ratios
  out[2] <- var(covar[trt_ind]) / var(covar[ctrl_ind])

  out <- t(out)
  colnames(out) <- c("St Mean Diff", "Var Ratio")
  return(out)
}

smd_vr <- function(covar, trt, wts = rep(1, length(covar)))
{
  #... covar is covariate (CONVERT UNORDERED FACTORS TO DUMMIES)
  #... trt is selection
  #... wts is weights

  covar <- as.numeric(as.character(covar)) #... ordered and 0-1 factors to num
  covar.by.sel <- split(covar, trt)
  wts.by.sel <- split(wts, trt)
  wtmn.0 <- weighted.mean(covar.by.sel[[1]], w = wts.by.sel[[1]], na.rm = TRUE)
  wtmn.1 <- weighted.mean(covar.by.sel[[2]], w = wts.by.sel[[2]], na.rm = TRUE)
  wt.mn.diff <- wtmn.1 - wtmn.0
  wtvar.0 <- cov.wt(matrix(covar.by.sel[[1]], length(covar.by.sel[[1]]), 1), wt = wts.by.sel[[1]])[[1]]
  wtvar.1 <- cov.wt(matrix(covar.by.sel[[2]], length(covar.by.sel[[2]]), 1), wt = wts.by.sel[[2]])[[1]]
  pooled.wt.var <- (wtvar.0*sum(wts.by.sel[[1]]) + wtvar.1*sum(wts.by.sel[[2]]))/sum(wts)
  smds <- (wt.mn.diff)/sqrt(pooled.wt.var)
  out <- t(c(smds, wtvar.1/wtvar.0))
  colnames(out) <- c("St Mean Diff", "Var Ratio")
  out
}
