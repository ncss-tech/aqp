## R functions to compute the weighted naive and tau statistics and display the results
## tauW : compute
## summaryTauW : display

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

#' Compute weighted naive and \emph{tau} statistics for a cross-classification matrix
#'
#' \code{tauW}: Computes: (1) unweighted naive, (2) weighted naive, (3) unweighted \emph{tau}, (4) weighted \emph{tau} accuracy statistics
#'
#' \code{summaryTauW}: prints a summary of the results from \emph{tauW}
#'
#' \code{xtableTauW}: formats a LaTeX table with results from \emph{tauW} and saves it as a \code{.tex} file for import into a LaTeX document.
#'
#' Input matrices \code{CM} and \code{W} may be in \code{data.frame} format and will be converted
#'
#' Weights matrix \code{W}: 0 = no credit; 1 = full credit; -1 = maximum penalty
#'
#' If absent, default is no partial credit, i.e., unweighted.
#'
#' Prior probabilities vector \code{P}: If absent, \code{P} are equal priors for each class. Special value \code{P = 0} is interpreted as \code{P} = column marginals.
#'
#' Error checks: \code{CM} must be square; \code{P} must have correct number of classes and sum to 1 +/- 0.0001; \code{W} & \code{CM} must be conformable
#'
#' @aliases tauW summaryTauW
#' @param CM a square confusion (cross-classification) matrix (rows: allocation, columns: reference)
#' @param W weights: 1 on diagonals, \[-1..1] off giving partial credit to this error
#' @param P prior probability vector, length = number of rows/columns in \code{CM} and \code{W}
#' @return Results are returned in a list with obvious R names
#' @author D G Rossiter
#'
#' @references
#'
#'    - Rossiter, D. G., Zeng, R., & Zhang, G.-L. (2017). Accounting for taxonomic distance in accuracy assessment of soil class predictions. Geoderma, 292, 118–127. \doi{10.1016/j.geoderma.2017.01.012}
#'
#'    - Ma, Z. K., & Redmond, R. L. (1995). Tau-coefficients for accuracy assessment of classification of remote-sensing data. Photogrammetric Engineering and Remote Sensing, 61(4), 435–439.
#'
#'    - Naesset, E. (1996). Conditional tau coefficient for assessment of producer’s accuracy of classified remotely sensed data. ISPRS Journal of Photogrammetry and Remote Sensing, 51(2), 91–98. \doi{10.1016/0924-2716(69)00007-4}
#'
#' @keywords array
#' @examples
#'
#' # example confusion matrix
#' # rows: allocation (user's counts)
#' # columns: reference (producer's counts)
#' crossclass <- matrix(data=c(2,1,0,5,0,0,
#'                             1,74,2,1,3,6,
#'                             0,5,8,6,1,3,
#'                             6,1,3,91,0,0,
#'                             0,4,0,0,0,4,
#'                             0,6,2,2,4,38),
#'                      nrow=6, byrow=TRUE)
#' row.names(crossclass) <- c("OP", "SA", "UA", "UC", "AV", "AC")
#' colnames(crossclass) <- row.names(crossclass)
#'
#' # build the weights matrix
#' # how much credit for a mis-allocation
#' weights <- matrix(data=c(1.00,0.05,0.05,0.15,0.05,0.15,
#'                          0.05,1.00,0.05,0.05,0.05,0.35,
#'                          0.05,0.05,1.00,0.20,0.15,0.15,
#'                          0.15,0.05,0.25,1.00,0.10,0.25,
#'                          0.05,0.10,0.15,0.10,1.00,0.15,
#'                          0.20,0.30,0.10,0.25,0.20,1.00),
#'                   nrow=6, byrow=TRUE)
#'
#' # unweighted accuracy
#' summaryTauW(nnaive <- tauW(crossclass))
#'
#' # unweighted tau with equal priors, equivalent to Foody (1992) modified Kappa
#' tauW(crossclass)$tau
#'
#' # unweighted tau with user's = producer's marginals, equivalent to original kappa
#' (priors <-  apply(crossclass, 2, sum)/sum(crossclass))
#' tauW(crossclass, P=priors)$tau
#'
#' # weighted accuracy; tau with equal priors
#' summaryTauW(weighted <- tauW(crossclass, W=weights))
#'
#' # weighted accuracy; tau with user's = producer's marginals
#' summaryTauW(tauW(crossclass, W=weights, P=priors))
#'
#' # change in accuracy statistics weighted vs. non-weighted
#' (weighted$overall.weighted - weighted$overall.naive)
#' (weighted$user.weighted - weighted$user.naive)
#' (weighted$prod.weighted - weighted$prod.naive)
#'
tauW <- function(CM,
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

summaryTauW <- function(result.tau) {
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

#' Format a LaTeX table with results
#'
#' @param result.tau results returned by \code{tauW}
#' @param file.name name of file to write output TeX file; Default: `file.name="tau_results_table.tex"`
#'
#' @return NULL
#' @export
#'
xtableTauW <- function(result.tau, file.name="tau_results_table.tex") {

  # safely check for required packages
  if(!requireNamespace('xtable', quietly = TRUE))
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
  cat(paste("Weighted \\emph{tau} $\\tau_w$: &", sprintf("%1.4f", result.tau$tauW), "\\\\"))
  cat("\\end{tabular}\n")
  cat("\\par\n")
  print(x)
  sink(file=NULL)
}
