

#' @title Shannon Entropy
#' @description A very simple implementation of Shannon entropy.
#'
#' @param x vector of probabilities [0,1], must sum to 1, should not contain NA
#' @param b logarithm base
#' 
#' @details `0`s are automatically removed by \code{na.rm = TRUE}, as \code{(0 * log(0) = Nan)}
#' 
#' @note When \code{b = length(x)} the result is the normalized Shannon entropy of (Kempen et al, 2009).
#'
#' @return A single numeric value.
#' 
#' @references 
#' Kempen, Bas, Dick J. Brus, Gerard B.M. Heuvelink, and Jetse J. Stoorvogel. 2009. "Updating the 1:50,000 Dutch Soil Map Using Legacy Soil Data: A Multinominal Logistic Regression Approach." Geoderma 151: 311-26. doi:10.1016/j.geoderma.2009.04.023
#' 
#' Shannon, Claude E. (July-October 1948). "A Mathematical Theory of Communication". Bell System Technical Journal. 27 (3): 379-423. doi:10.1002/j.1538-7305.1948.tb01338.x
#' 
#' 
#' @export
#'
#' @examples
#' 
#' # a very simple example
#' p <- c(0.25, 0.25, 0.4, 0.05, 0.05)
#' 
#' shannonEntropy(p)
#' 
#' 
#' 
## TODO: test that sum(x) == 1
shannonEntropy <- function(x, b = 2) {
  # 0s automatically removed by na.rm=TRUE (0 * log(0) = Nan)
  res <- -1 * sum(x * log(x, base = b), na.rm = TRUE)
  return(res)
}



#' @title Confusion Index
#' 
#' @description Calculate the confusion index of Burrough et al., 1997.
#'
#' @param x vector of probabilities [0,1], should not contain NA
#' 
#' @author D.E. Beaudette
#' 
#' @references Burrough, P.A., P.F.M. van Gaans, and R. Hootsmans. 1997. "Continuous Classification in Soil Survey: Spatial Correlation, Confusion and Boundaries." Geoderma 77: 115-35. doi:10.1016/S0016-7061(97)00018-9.
#' 
#' @return A single numeric value.
#' @export
#'
#' @examples
#' 
#' # a very simple example
#' p <- c(0.25, 0.25, 0.4, 0.05, 0.05)
#' confusionIndex(p)
#' 
#' # for comparison
#' shannonEntropy(p)
#' 
confusionIndex <- function(x) {
  x <- sort(x, decreasing = TRUE)
  res <- 1 - (x[1] - x[2])
  return(res)
}

# multinominal Brier score
# x: data.frame, rows are predictions/observations, columns contain classes
# classLabels: vector of class labels, corrosponding to column names in x.i
# actual: name of column containing the observed class

#' @title Multinominal Brier Score
#' 
#' @description Compute a multinominal Brier score from predicted class probabilities and observed class label. Lower values are associated with a more accurate classifier.
#'
#' @param x \code{data.frame} of class probabilities (numeric) and observed class label (character), see examples
#' @param classLabels vector of predicted class labels (probabilities), corrosponding to column names in \code{x}
#' @param actual name of column containing the observed class, should be character vector not factor
#' 
#' @references Brier, GLenn W. 1950. "Verification of Forecasts Expressed in Terms of Probability." Monthly Weather Review 78 (1): 1-3. doi:10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2.
#' 
#' @author D.E. Beaudette
#' 
#' @return a single Brier score, representative of data in \data{x}
#' @export
#'
#' @examples
#' 
#' # columns 'a', 'b', 'c' contain predicted probabilities
#' # column 'actual' contains observed class label
#' 
#' # a good classifier
#' d.good <- data.frame(
#'   a = c(0.05, 0.05, 0.10),
#'   b = c(0.90, 0.85, 0.75),
#'   c = c(0.05, 0.10, 0.15),
#'   actual = c('b', 'b', 'b'),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # a rather bad classifier
#' d.bad <- data.frame(
#'   a = c(0.05, 0.05, 0.10),
#'   b = c(0.90, 0.85, 0.75),
#'   c = c(0.05, 0.10, 0.15),
#'   actual = c('c', 'c', 'c'),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # class labels are factors
#' d.factors <- data.frame(
#'   a = c(0.05, 0.05, 0.10),
#'   b = c(0.90, 0.85, 0.75),
#'   c = c(0.05, 0.10, 0.15),
#'   actual = c('b', 'b', 'b'),
#'   stringsAsFactors = TRUE
#' )
#' 
#' # relatively low value = accurate
#' brierScore(x = d.good, classLabels = c('a', 'b', 'c'), actual = 'actual')
#' 
#' # high values = not accuate
#' brierScore(x = d.bad, classLabels = c('a', 'b', 'c'), actual = 'actual')
#' 
#' # message related to conversion of factor -> character
#' brierScore(x = d.factors, classLabels = c('a', 'b', 'c'), actual = 'actual')
#' 
brierScore <- function(x, classLabels, actual = 'actual') {
  # number of observations
  n <- nrow(x)
  
  # sanity check: no factors allowed in class labels
  if(inherits(x[[actual]], 'factor')) {
    message('converting `actual` from factor to character')
    x[[actual]] <- as.character(x[[actual]])
  }
  
  # extract vector of observed classes
  x.actual <- x[[actual]]
  
  # keep only probabilities as matrix
  x <- as.matrix(x[, classLabels, drop=FALSE])
  
  # init new matrix to store most-likely class
  m <- matrix(0, ncol = ncol(x), nrow = n)
  # same structure as x.pr
  dimnames(m)[[2]] <- classLabels
  
  # set cells of actual observed outcome to 1
  for(i in 1:n) {
    x.i <- x.actual[i]
    m[i, x.i] <- 1
  }
  
  # compute multinominal brier score
  # 1/n * sum((x - m)^2)
  # x: matrix of predictions
  # m: indicator matrix of outcomes
  bs <- (1/n) * sum((x - m)^2, na.rm=TRUE)
  return(bs)
}

