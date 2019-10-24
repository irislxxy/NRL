### Functions to get SMD and var ratios on a covariate
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
    text(output, labels = rownames(output), adj = c(0, -.3))
  }
  output
}

### Function for continuous variables
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
  hist(lps[trt==1], breaks = brks, xlab = "", main = "Treated Cases",
       freq = FALSE)
  d1 <- density(lps[trt==1])
  lines(d1, col = 2, lwd = 1, lty = 1)
  abline(v = c(range(lps[trt==1])), col = 3, lty = 2)
  hist(lps[trt==0], breaks = brks, xlab = "", main = "Control Cases",
       freq = FALSE)
  d2 <- density(lps[trt==0])
  lines(d2, col = 2, lwd = 1, lty = 1)
  abline(v = c(range(lps[trt==0])), col = 3, lty = 2)
  par(mfrow = c(1,1))
}

### Note that ovlp_ind requires that package optmatch be loaded
ovlp_ind <- function(trt, lps, caliper = 0.1)
{
  nt <- sum(trt == 1); nc <- sum(trt == 0)
  SDpool <- sqrt( ( (nt - 1)*var(lps[trt == 1]) + (nc - 1)*var(lps[trt == 0]) ) / 
                  (nt + nc - 2) )
  ### Get abs(distance) for each treat/control pairing
  diffs <- match_on(trt ~ lps, method = "euclidean")
  smds <- diffs/SDpool # standardize differences by dividing by pooled SD
  fun <- function(vec) {min(vec) <= caliper}
  trtOvlp <- apply(smds, 1, fun)   # TRUEs are overlapping
  ctrlOvlp <- apply(smds, 2, fun)  # FALSEs are not
  drop1 <- which(trt==1)[!trtOvlp]
  drop0 <- which(trt==0)[!ctrlOvlp]
  ind <- !logical(length(lps))
  ind[c(drop1, drop0)] <- FALSE
  ind
}

var_wt <- function(x, wt)
{
  wt_m <- x %*% wt / sum(wt)
  wt_var <- x^2 %*% wt / sum(wt) - m^2
  out <- c(wt_m, wt_var)
  out
}

