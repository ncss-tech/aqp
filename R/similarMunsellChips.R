#' @title Indices of "equivalent" Munsell chips in \code{munsell} data set
#'
#' @description
#' A pre-calculated lookup list (made with \code{farver::compare_colour}) based on pair-wise CIE2000 color contrast (\code{dE00}) of LAB colors with D65 illuminant for all whole value/chroma "chips" in the \code{aqp::munsell} data set.
#' 
#' The intention is to identify Munsell chips that may be "functionally equivalent" to some other given whole chip elsewhere in the Munsell color space -- as discretized in the \code{aqp::munsell} lookup table. 
#'
#' "Equivalent" chips are based (fairly arbitrarily) on the 0.001 probability level of dE00 (default Type 7 \code{quantile}) within the upper triangle of the 8467x8467 contrast matrix. This corresponds to a \code{dE00} contrast threshold of approximately 2.15. 
#' 
#' This is a naive (to the subtleties of human color perception, and overall magnitude of contrast between some of the "chips") but computationally consistent approach. Using the lookup list, as opposed to manual contrast via e.g. \code{farver::compare_colour} may have some benefits for efficiency in certain applications where the exact contrast value is not as important as the concept of having some threshold that is non-zero, but very small.
#'  
#' @usage data(munequivalent)
#'
#' @seealso \code{\link{equivalentMunsellChips}}
#' 
#' @references
#' Gaurav Sharma, Wencheng Wu, Edul N. Dalal. (2005). The CIEDE2000 Color-Difference Formula: Implementation Notes, Supplementary Test Data, and Mathematical Observations. COLOR research and application. 30(1):21-30. http://www2.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf
#' 
#'  Thomas Lin Pedersen, Berendea Nicolae and Romain François (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
#'  
#' Dong, C.E., Webb, J.B., Bottrell, M.C., Saginor, I., Lee, B.D. and Stern, L.A. (2020). Strengths, Limitations, and Recommendations for Instrumental Color Measurement in Forensic Soil Characterization. J Forensic Sci, 65: 438-449. https://doi.org/10.1111/1556-4029.14193
#' 
#' @aliases munequivalent
#'
#' @keywords datasets
#'
#' @format A named list with 8467 elements, each containing a numeric vector of indices corresponding to the \code{munsell} data set, which has 8467 rows (unique, whole-number chips). Names have the format \code{HUE VALUE/CHROMA}, eg. \code{"7.5YR 4/4"}
"munequivalent"

#' .makeEquivalentMunsellLUT
#'
#' @description Makes the look up table based on pair-wise CIE2000 color contrast (\code{dE00}) of LAB colors with D65 illuminant of LAB colors for all whole value/chroma "chips" in the \code{aqp::munsell} data set via \code{farver::compare_colour}. Specify a threshold in terms of a probability level of CIE2000 distance (relative to whole dataset).

#' @param threshold Quantile cutoff (of dE00 color contrast) for "equivalent" colors, default \code{0.001} based on all whole value/chroma "chips" in the \code{munsell} data set.
#' 
#' @seealso \code{\link{equivalentMunsellChips}}
#' 
#' @references
#' Gaurav Sharma, Wencheng Wu, Edul N. Dalal. (2005). The CIEDE2000 Color-Difference Formula: Implementation Notes, Supplementary Test Data, and Mathematical Observations. COLOR research and application. 30(1):21-30. http://www2.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf
#' 
#'  Thomas Lin Pedersen, Berendea Nicolae and Romain François (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
#'  
#' Dong, C.E., Webb, J.B., Bottrell, M.C., Saginor, I., Lee, B.D. and Stern, L.A. (2020). Strengths, Limitations, and Recommendations for Instrumental Color Measurement in Forensic Soil Characterization. J Forensic Sci, 65: 438-449. https://doi.org/10.1111/1556-4029.14193
#' 
#' @return A list with equal length to the number of rows (chips) in \code{munsell} data, each containing a numeric vector of row indices that are equivalent (CIE2000 distance less than \code{threshold}).
#'
#' @examples
#' \dontrun{
#'
#'   # use default threshold
#'   equivalent_chip_lut <- .makeEquivalentMunsellLUT()
#'
#'   # inspect the first 10 chips, it seems to work!
#'   lapply(equivalent_chip_lut[1:10], function(i) munsell[i,])
#'
#'   # lets see some info on the number of chips per chip
#'   nchipsper <- sapply(equivalent_chip_lut, length)
#'
#'   # top 10 are very high chroma chips with over 70 chips "identical"
#'   nchipsper[order(nchipsper, decreasing = TRUE)[1:10]]
#'
#'   # look at distribution
#'   plot(density(nchipsper))
#'
#'   # median is 5 -- Q: is this true of the range of Munsell colors typically used for soils?
#'   quantile(nchipsper)
#'
#'   # double the default threshold
#'   doubletest <- sapply(.makeEquivalentMunsellLUT(threshold = 0.002), length)
#'   lines(density(doubletest), lty=2)
#'
#'   # apprx. doubles the number of chips per chip in IQR
#'   quantile(doubletest)
#' }
#'
#' @importFrom farver compare_colour
#'
.makeEquivalentMunsellLUT <- function(threshold = 0.001) {
  munsell <- NULL
  load(system.file("data/munsell.rda", package = "aqp")[1])

  # this produces an 8467x8467 matrix ~285MB; just want upper triangle no diagonal
  # # thanks to @dylanbeaudette for pointing out better farver syntax
  # system.time(x <- farver::compare_colour(munsell[, c('L', 'A', 'B')],
  #                                         from_space='lab', white_from = 'D65', method='cie2000'))
  # # # takes about a half minute to run
  # # # user  system elapsed
  # # # 31.465   0.364  31.913
  #
  # x[lower.tri(x, diag = TRUE)] <- NA # convert 0 to NA to ignore in stats
  # xdat <- x

  # TODO: ... some unholy indexry I cant quite figure out; the stats are right but order is wrong

  # this one that takes 2x as long to built the LUT, and is 2x as big in memory
  (x <- farver::compare_colour(from = munsell[,c('L','A','B')], from_space = 'lab',
                              to = munsell[,c('L','A','B')], to_space = 'lab',
                              method = 'cie2000', white_from = 'D65', white_to = 'D65'))
  xdat <- x
  x[lower.tri(x, diag = TRUE)] <- NA 
  # remove lower triangle for statistics (only count each pair distance 1x)

  # calculate quantiles
  xqtl <- quantile(x, p = threshold, na.rm = TRUE)[1]

  # storing just the chips that fall within the closest e.g. tenth of a percentile: ~1.4MB in memory as list
  # 
  # TODO: consider applications of a matrix (rather than vector) at defined dE00 levels
  # 
  xin1 <- apply(xdat, 1, function(xrow) as.integer(which(xrow <= xqtl)))

  # # ALTERNATIVE THRESHOLDS
  # ## calculate unique threshold for each row (results in way too many "similar" chips)
  # rowthresholds <- apply(x, 1, function(xrow) quantile(xrow, p = threshold, na.rm = TRUE)[1])
  # xin2 <- lapply(1:nrow(x), function(i) as.integer(which(xdat[i,] <= rowthresholds[i])))
  #
  # ## mean of row-wise quantiles - slightly higher than the global threshold
  # rowthreshbar <- mean(apply(x, 1, function(xrow) quantile(xrow, p = threshold, na.rm = TRUE)[1]), na.rm=TRUE)
  # xin3 <- apply(xdat, 1, function(xrow) as.integer(which(xrow <= rowthreshbar)))
  #
  # ## DEBUG plots
  # # inspect the distribution of dE00 and threshold values
  # par(mfrow=c(2,1))
  # plot(density(x, na.rm=TRUE, from=0), xlim=c(0,130),
  #      main = "Between-whole-chip dE00 -- aqp::munsell pair-wise differences",
  #      sub = sprintf("Dotted vertical lines denote row-wise quantiles @ prob=%s", threshold))
  #
  # # # inspect visual cutoff by method 1 and 3
  # abline(v = rowthresholds, lty=3, col=rgb(0,0,0,0.1))# rowwise quantile at threshold
  #
  # # # row-wise quantiles
  # plot(density(rowthresholds, na.rm=TRUE, from=0), lty = 3, xlim = c(0,130),
  #      main = sprintf("Threshold for 'equivalent' whole-chip dE00 @ prob=%s", threshold))
  # abline(v = xqtl[1], lty=1, lwd=2, col="red")
  # abline(v = rowthreshbar, lty=3, lwd=2, col="blue")  #
  # legend("topright", legend = c(sprintf("Density plot of row-wise dE00 quantiles (@ prob=%s)",
  #                                       threshold),
  #                               sprintf("Global quantile (dE00=%s @ prob=%s)",
  #                                       round(xqtl[1], 2), threshold),
  #                               sprintf("Global mean of row-wise quantile (dE00=%s @ prob=%s)",
  #                                       round(rowthreshbar,2), threshold)),
  #        lty = c(3,1,3), lwd = c(1,2,2), col = c("BLACK","RED","BLUE"))

  # # number of chips per chip
  # par(mfrow=c(1,1))
  # plot(density(sapply(xin1, length), bw=1))
  # lines(density(sapply(xin2, length), bw=1), lty=2)
  # lines(density(sapply(xin3, length), bw=1), lty=3)

  # create a nice lookup table to add to aqp
  munequivalent <- xin1
  names(munequivalent) <- sprintf("%s %s/%s", munsell$hue, munsell$value, munsell$chroma)

  # this is only 90kB written to Rda
  # save(munequivalent, file="data/equivalent_munsell.rda")

  return(munequivalent)
}

#' Identify "equivalent" (whole number value/chroma) Munsell chips
#'
#' @description 
#' 
#' Uses a pre-calculated lookup list (\code{\link{munequivalent}}) based on pair-wise CIE2000 contrast (\code{dE00}) of LAB color with D65 illuminant for all whole value/chroma "chips" in the \code{aqp::munsell} data set. 
#' 
#' The intention is to identify Munsell chips that may be "functionally equivalent" to some other given whole value/chroma chip elsewhere in the Munsell color space -- as discretized in the \code{aqp::munsell} data table. This basic assumption needs to be validated against your end goal: probably by visual inspection of some or all of the resulting sets. See \code{\link{colorContrast}} and \code{\link{colorContrastPlot}}.
#'
#' "Equivalent" chips table are based (fairly arbitrarily) on the 0.001 probability level of dE00 (default Type 7 \code{quantile}) within the upper triangle of the 8467x8467 contrast matrix. This corresponds to a \code{dE00} contrast threshold of approximately 2.15. 

#' @param hue A character vector containing Munsell hues
#' @param value A numeric vector containing Munsell values (integer only)
#' @param chroma A numeric vector containing Munsell chromas (integer only)
#'
#' @return A named list; Each list element contains a data.frame with one or more rows of "equivalent" Munsell, RGB and LAB color coordinates from \code{munsell} data set.
#' 
#' @seealso \code{\link{colorContrast}} \code{\link{colorContrastPlot}} \code{\link{munequivalent}} 
#' @references
#' 
#' Gaurav Sharma, Wencheng Wu, Edul N. Dalal. (2005). The CIEDE2000 Color-Difference Formula: Implementation Notes, Supplementary Test Data, and Mathematical Observations. COLOR research and application. 30(1):21-30. http://www2.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf
#' 
#' Thomas Lin Pedersen, Berendea Nicolae and Romain François (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
#'  
#' Dong, C.E., Webb, J.B., Bottrell, M.C., Saginor, I., Lee, B.D. and Stern, L.A. (2020). Strengths, Limitations, and Recommendations for Instrumental Color Measurement in Forensic Soil Characterization. J Forensic Sci, 65: 438-449. https://doi.org/10.1111/1556-4029.14193
#'
#' @export
#'
#' @examples
#'
#' # 7.5YR 4/4 (the one and only)
#' 
#' equivalentMunsellChips("7.5YR", 4, 4)
#' #>
#' #> $`7.5YR 4/4`
#' #>        hue value chroma         r        g         b        L       A       B
#' #> 8330 7.5YR     4      4 0.4923909 0.352334 0.2313328 41.26403 10.8689 23.5914
#' 
#' # 7.5YR 1/1 (two chips are equivalent; 3 row result)
#' 
#' equivalentMunsellChips("7.5YR", 1, 1)
#' #>
#' #> $`7.5YR 1/1`
#' #>        hue value chroma         r         g          b        L        A        B
#' #> 1983  10YR     1      1 0.1345633 0.1087014 0.07606787 10.64787 1.621323 6.847629
#' #> 6189   5YR     1      1 0.1330994 0.1076359 0.09450179 10.63901 2.489012 3.515146
#' #> 8303 7.5YR     1      1 0.1329483 0.1082380 0.08862581 10.64210 2.065514 4.623922
#' 
#' # 10YR 6/8 (two chips are equivalent; 3 row result)
#' 
#' equivalentMunsellChips("10YR", 6, 8)
#' #>
#' #> $`10YR 6/8`
#' #>       hue value chroma         r         g         b        L        A        B
#' #> 2039 10YR     6      7 0.7382230 0.5512957 0.2680260 61.76795 10.50886 44.78574
#' #> 2040 10YR     6      8 0.7519872 0.5472116 0.2157209 61.77496 11.83215 51.15496
#' #> 2041 10YR     6      9 0.7642826 0.5433189 0.1559069 61.78085 13.09599 57.49773
#' 
#' # compare visually a very red color
#' 
#' veryred <- equivalentMunsellChips("10R", 6, 28)[[1]]
#' 
#' par(mar=c(0,0,1,1))
#' 
#' pie(rep(1, nrow(veryred)), col = with(veryred, munsell2rgb(hue, value, chroma)),
#'     label = with(veryred, sprintf("%s %s/%s", hue, value, chroma)))
#' 
#' table(veryred$hue) # 2 hues
#' #> 
#' #>  10R 7.5R 
#' #>    8   17
#' 
#' table(veryred$value) # 2 values
#' #> 
#' #>  5  6 
#' #> 11 14
#' 
#' table(veryred$chroma) # 10 chromas
#' #> 
#' #> 21 22 23 24 25 26 27 28 29 30 
#' #>  1  2  2  3  3  4  3  3  2  2
#'
equivalentMunsellChips <- function(hue = NULL, value = NULL, chroma = NULL) {

  munequivalent <- NULL
  load(system.file("data/equivalent_munsell.rda", package = "aqp")[1])

  munsell <- NULL
  load(system.file("data/munsell.rda", package = "aqp")[1])

  chipdata <- data.frame(.id = 1:pmax(length(hue), length(value), length(chroma)),
                         hue = hue, value = value, chroma = chroma)

  lidx <- lapply(chipdata$.id, function(x) {
    # TODO: handle half values? e.g. 7.5YR 2.5/2?
    which(munsell$hue == chipdata$hue[x] &
            munsell$value == chipdata$value[x] &
            munsell$chroma == chipdata$chroma[x])
  })
  res <- lapply(lidx, function(i) munsell[munequivalent[i][[1]],])
  names(res) <- sprintf("%s %s/%s", hue, value, chroma)
  rownames(res) <- NULL
  return(res)
}

