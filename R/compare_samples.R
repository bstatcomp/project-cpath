compare_samples <- function (samp1, samp2, statistic = "mean") {
  get_mcmcse <- function (samp) {
    out <- mcse(samp)
    return(out$se)
  }
  if (statistic == "mean") {
    mu1     <- apply(samp1, 2, mean)
    mu2     <- apply(samp2, 2, mean)
  } else if (statistic == "median") {
    mu1     <- apply(samp1, 2, median)
    mu2     <- apply(samp2, 2, median)
  } else {
    stop("Statistic must be either mean or median.")
  }
  mcmcse1 <- apply(samp1, 2, get_mcmcse)
  mcmcse2 <- apply(samp2, 2, get_mcmcse)
  is_within <- abs(mu1 - mu2) <= 2 * mcmcse1
  diff_perc <- abs(mu1 - mu2) / abs(mu1)
  out <- data.frame("expected1" = mu1,
                    "expected2" = mu2,
                    "mcmcse1"   = mcmcse1,
                    "mcmcse2"   = mcmcse2,
                    "is_same"   = is_within,
                    "difference" = abs(mu1 - mu2))
  return(out)
}