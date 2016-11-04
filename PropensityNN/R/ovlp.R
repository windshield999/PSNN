#'Plot the histogram for each group, examining if the observations' PS/PS logit distributions are overlapped.
#'@param trt Treatment assignment
#'@param lps Logit propensity score
#'@export


ovlp <- function(trt, lps)
{
  ### This function displays an overlap histogram for "lps" across "trt" groups
  ### Statistical test of mean difference
  tt <- t.test(x = lps[trt==1], y = lps[trt==0], alternative = "two.sided",
               var.equal = FALSE)$p.value
  ### Statistical test of difference in distribution
  ks <- ks.test(x = lps[trt==1], y = lps[trt==0], alternative = "two.sided")$p.value

  par(mfrow = c(2,1))
  rc <- range(lps)
  brks <- seq(from = rc[1] - diff(rc)/20, to = rc[2] + diff(rc)/20, by = diff(rc)/20)
  hist(lps[trt==1], breaks = brks, xlab = "PS logit", main = "Treated Cases",
       freq = FALSE)
  d1 <- density(lps[trt==1])
  lines(d1, col = 2, lwd = 1, lty = 1)
  abline(v = c(range(lps[trt==1])), col = 3, lty = 2)
  hist(lps[trt==0], breaks = brks, xlab = "PS logit", main = "Control Cases",
       freq = FALSE)
  d2 <- density(lps[trt==0])
  lines(d2, col = 2, lwd = 1, lty = 1)
  abline(v = c(range(lps[trt==0])), col = 3, lty = 2)
  par(mfrow = c(1,1))
}
