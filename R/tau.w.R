## R functions to compute the weighted naive and tau statistics and display the results
## tau.w : compute
## summary.tau.w : display

## R function to compute the weighted naive and tau statistics
##
## Arguments:
##   CM: a square confusion (cross-classification) matrix
##       (rows: allocation, columns: reference)
##   W : weights; 1 on diagonals, [-1..1] off giving partial credit to this error
##       0 = no credit; 1 = full credit; -1 = maximum penalty
##       If absent, default is no partial credit, i.e., unweighted
##   P : prior probability vector, length = number of rows/columns in CM and W
##	 If absent, P are equal priors for each class
##       Special value P = 0 is interpreted as P = column marginals
##
## Input matrices may be in data.frame format and will be converted
##
## Computes: (1) unweighted naive, (2) weighted naive, (3) unweighted tau, (4) weighted tau
##
## Results are returned in a list with obvious R names
## Error checks: CM must be square; P must have correct number of classes
##	and sum to 1 +/- 0.0001; W & CM must be same size
#
## Author: D G Rossiter, 26-Dec-2016, Ithaca, NY (USA)
#
tau.w <- function(CM,
                W = diag(sqrt(length(as.matrix(CM)))),
                P = rep(1/nrow(as.matrix(CM)), nrow(as.matrix(CM)))) {

    ## convert both data frames and vectors to matrices
  cmx <- as.matrix(CM); wx <- as.matrix(W); pv <- as.vector(P)
  
  ## convert a 1-D vector to a square matrix
  if (ncol(cmx) == 1)
    cmx <- matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  if (ncol(wx) == 1)
    wx <- matrix(wx, byrow=TRUE, nrow=sqrt(nrow(wx)))
  nr <- nrow(cmx); nc <- ncol(cmx)
  
  ## check CM
  if (nr != nc) {
    print("Error: cross-classification matrix is not square")
    return(NULL)
  }

    ## check W
  if (dim(wx)[1] != dim(wx)[2]) {
    print("Error: weights matrix is not square")
    return(NULL)
  }
  if (dim(wx)[1] != dim(cmx)[1]) {
    print("Weight and Confusion Matrices are not the same size")
    return(NULL)
  }
  if ((any(wx > 1)) || (any(wx < -1))) {
    print("All weights must be on [-1..1]")
    return(NULL)
  }
  
  ## totals of matrix, rows and columns
  n <- sum(cmx); rsum = apply(cmx, 1, sum); csum = apply(cmx, 2, sum)
  
  ## if P is special value 0, set to reference distribution
  if ((length(pv))==1 && (pv == 0)) {
    print("Setting prior probabilities to reference class distribution")
    pv <- csum/n
  }
  
  ## check P
  if (length(pv) != nc) {
    print("Error: number of prior probabilities not equal to number of classes")
    return(NULL)
  }
  if (abs(1-sum(pv)) > 0.0001) {
    print("Error: prior probabilities must sum to 1")
    return(NULL)
  }
  
  ## make class names consistent among objects
  row.names(cmx) <- names(cmx)
  row.names(wx) <- names(wx)
  names(pv) <- names(cmx)
  
  ## compute unweighted naive statistics
  d <- diag(cmx); dsum <- sum(d); oa <- dsum/n
  ua <- d/rsum; pa <- d/csum
  names(ua) <- names(pa)
  
  ## compute weighted naive statistics
  ##
  
  ## confusion matrix and marginals as proportions
  cmxp <- cmx/n; cp <- csum/n; rp <- rsum/n;
  
  ## expected proportions
  pp<- rp %o% cp
  
  ## weighted weights
  ## wr <- wx %*% cp; wc <- t(t(wx) %*% rp)
  ## overall weighted accuracy
  oaw <- sum(wx * cmxp)
  
  ## marginal weighted accuracy
  uaw <- apply(wx * cmxp, 1, sum)/rp
  paw <- apply(wx * cmxp, 2, sum)/cp
  names(uaw) <- names(paw)
  
  ## compute unweighted tau
  ##
  th1 <- sum(diag(cmxp))
  th2 <- sum(pv %*% (csum/n))
  tau <- (th1-th2)/(1-th2);
  
  ## compute weighted tau
  ##
  thw1 <- sum(wx * cmxp)
  thw2 <- sum(wx * (pv %o% (csum/n)))
  tau.w <- (thw1-thw2)/(1-thw2);
  
  
  ## return list of results
  return(list(crossclass = cmx, weights=wx,
              obs=rsum, ref=csum, n=n,
              overall.naive=oa,
              user.naive=ua, prod.naive=pa,
              overall.weighted=oaw,
              user.weighted=uaw, prod.weighted=paw,
              tau.priors=pv,
              tau=tau,
              tau.w=tau.w))
}

## Print a summary of the various accuracy statistics
## Argument: result.tau   as returned from function tau.w
##
summary.tau.w <- function(result.tau) {
  print("Cross-classification matrix:", quote=F)
  print(result.tau$crossclass)
  print(paste("Number of observations:", result.tau$n), quote=F)
  print("Weights:", quote=F)
  print(result.tau$weights, quote=F)
  
  print(paste("Overall accuracy (unweighted):",
              round(result.tau$overall.naive,4)), quote=F)
  print(paste("Overall accuracy (weighted):",
              round(result.tau$overall.weighted,4)), quote=F)
  
  print("User's accuracy (unweighted):", quote=F)
  print(round(result.tau$user.naive,4))
  print("User's accuracy (weighted):", quote=F)
  print(round(result.tau$user.weighted,4))
  print("Producer's reliability (unweighted):", quote=F)
  print(round(result.tau$prod.naive,4))
  print("Producer's reliability (weighted):", quote=F)
  print(round(result.tau$prod.weighted,4))
  
  print("Reference class proportions:", quote=F)
  print(round(result.tau$ref/result.tau$n,4), quote=F)
  print("Observed class proportions:", quote=F)
  print(round(result.tau$obs/result.tau$n,4), quote=F)
  print("Prior class probabilities:", quote=F)
  print(result.tau$tau.priors, quote=F)
  print(paste("Tau (unweighted):", round(result.tau$tau,4)), quote=F)
  print(paste("Tau (weighted):", round(result.tau$tau.w,4)), quote=F)
}

## Format a LaTeX table with results
## then \input into your LaTeX document
## Arguments
##   result.tau   result of running tau.w
##   file.name    name of file to write
xtable.tau.w <- function(result.tau, file.name="tau_results_table.tex") {
  
  # safely check for required packages
  if(!requireNamespace('xtable'))
    stop('this function requires the `xtable` package.', call.=FALSE)
  
  options(xtable.floating = FALSE)
  options(xtable.timestamp = "")
  tab <- data.frame(result.tau$user.naive, result.tau$user.weighted,
                    result.tau$prod.naive, result.tau$prod.weighted)
  names(tab) <- c("UA", "UA(W)", "PA", "PA(W)")
  x <- xtable::xtable(tab)
  xtable::autoformat(x)
  sink(file=file.name)
  cat("\\begin{tabular}{rl}\n")
  cat(paste("Unweighted accuracy $A_o$: &", sprintf("%1.4f", result.tau$overall.naive), "\\\\"))
  cat(paste("Weighted accuracy ${A_o}_w$: &", sprintf("%1.4f", result.tau$overall.weighted), "\\\\"))
  cat(paste("Unweighted \\emph{tau} $\\tau$: &", sprintf("%1.4f", result.tau$tau), "\\\\"))
  cat(paste("Weighted \\emph{tau} $\\tau_w$: &", sprintf("%1.4f", result.tau$tau.w), "\\\\"))
  cat("\\end{tabular}\n")
  cat("\\par\n")
  print(x)
  sink(file=NULL)
}
