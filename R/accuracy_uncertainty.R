
#' @title Shannon Entropy
#' @description A very simple implementation of Shannon entropy.
#'
#' @param x vector of probabilities (0,1), must sum to 1, should not contain NA
#' @param b logarithm base
#' 
#' @details `0`s are automatically removed by `na.rm = TRUE`, as `(0 * log(0) = Nan)`
#' 
#' @note When `b = length(x)` the result is the normalized Shannon entropy of (Kempen et al, 2009).
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
  # consider: ifelse(x == 0, 0, x * log(x))
  
  res <- -1 * sum(x * log(x, base = b), na.rm = TRUE)
  return(res)
}



#' @title Confusion Index
#' 
#' @description Calculate the confusion index of Burrough et al., 1997.
#'
#' @param x vector of probabilities (0,1), should not contain NA
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



#' @title Multinominal Brier Score
#' 
#' @description Compute a multinominal Brier score from predicted class probabilities and observed class label. Lower values are associated with a more accurate classifier.
#'
#' @param x `data.frame` of class probabilities (numeric) and observed class label (character), see examples
#' 
#' @param classLabels vector of predicted class labels (probabilities), corresponding to column names in `x`
#' @param actual name of column containing the observed class, should be character vector not factor
#' 
#' @references Brier, Glenn W. 1950. "Verification of Forecasts Expressed in Terms of Probability." Monthly Weather Review 78 (1): 1-3. doi:10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2.
#' 
#' @author D.E. Beaudette
#' 
#' @return a single Brier score, representative of data in `x`
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
  if (inherits(x, 'data.frame')) {
    x <- data.frame(x)
  } else stop("`x` should be a data.frame", call. = FALSE)
  
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
  x <- as.matrix(x[, classLabels, drop = FALSE])
  
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
  bs <- (1/n) * sum((x - m)^2, na.rm = TRUE)
  return(bs)
}


#' Map unit confusion matrix and other classification measures
#' 
#' This function reverse engineers a confusion matrix and other classification measures from soil map unit component percentages (i.e. composition) and area (i.e. acres).
#' @param x `data.frame`
#' @param mapunit `character` column name containing the mapunit identifier (e.g. nationalmusym)
#' @param cophase `character` column name containing the soil component phase identifier (e.g. coiid or paste(compname, localphase))
#' @param comppct `character` column name containint the component percent (e.g. comppct_r)
#' @param muacres `character` column name containing the total area of the mapunit (e.g. muacres)
#' 
#' @details `mu_confusion_matrix` There are several common statistical measures used to gauge the accuracy of categorical maps. These measures are typically not estimated for soil surveys but can be inferred from a map unit’s soil component composition percentages and size (i.e. acres). In general, overall purity or accuracy (OP) is related to map unit kind (e.g. consociations vs complexes). For several reasons, the “true” accuracies are unknown, and these values should be interpreted as Bayesian prior estimates. However, it is likely that the estimates are optimistic if they haven't been derived from an external validation. Existing and future digital soil mapping products could be used to validate how optimistic the current OA estimates are.
#'
#' @return `list` a confusion matrix, overall purity (OP) (i.e. overall accuracy), map unit purity (MP) (i.e. user accuracy), class representation (CR) (i.e. producer accuracy), and Shannon entropy (E). The measure names were selected to be consistent with the alternative terms proposed by Lark (1995) and Brus (2011). 
#' 
#' @author Stephen Roecker
#' 
#' @references
#'
#'    - Brus DJ, Kempen B, Heuvelink GBM. 2011. Sampling for validation of digital soil maps. European Journal of Soil Science. 62(3):394–407. \doi{10.1111/j.1365-2389.2011.01364.x}
#'    - Lark RM. 1995. Components of accuracy of maps with special reference to discriminant analysis on remote sensor data. International Journal of Remote Sensing. 16(8):1461–1480. \doi{10.1080/01431169508954488}
#' 
#' @seealso [tauW()], [shannonEntropy()], [confusionIndex()]
#' 
#' @export
#'
#' @examples
#' 
#' # example data
#' mu <- rbind(
#'     data.frame(mapunit = "A", cophase = c("Alpha", "Beta"), comppct = c(90, 10), muacres = 100),
#'     data.frame(mapunit = "B", cophase = c("Beta", "Alpha"), comppct = c(70, 30), muacres = 1000)
#'     )
#' 
#' mu_confusion_matrix(mu, mapunit = "mapunit", cophase = "cophase", comppct = "comppct")
#' 

mu_confusion_matrix <- function(x, mapunit = "nationalmusym", cophase = "coiid", comppct = "comppct_r", muacres = "muacres") {
  
  vars <- c(mapunit = mapunit, cophase = cophase, comppct = comppct, muacres = muacres)
  idx  <- sapply(vars, function(y) which(names(x) %in% y))
  names(x)[idx] <- names(vars)
  x <- x[names(vars)]
  
  x$coacres <- x$muacres * x$comppct/100
  
  CM <- stats::xtabs(coacres ~ mapunit + cophase, data = x)
  rowmax <- apply(CM, 1, max)
  colmax <- apply(CM, 2, max)
  MP <- rowmax / rowSums(CM)
  CR <- colmax / colSums(CM)
  OP <- sum(rowmax) / sum(CM)
  H  <- apply(CM, 1, function(x) aqp::shannonEntropy(x/sum(x)))
  # CM <- CM |> cbind(MP, H) |>  rbind(CR = c(CR, c(NA, NA)))
  names(dimnames(CM)) <- c("map unit", "component phase")
  
  return(
    list(
      confusion_matrix     = CM |> round(2), 
      overall_purity       = OP, 
      map_purity           = MP, 
      class_representation = CR, 
      shannon_entropy      = H)
  )
}



