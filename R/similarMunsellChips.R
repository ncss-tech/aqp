#'
#' @title Indices of "equivalent" Munsell chips in \code{munsell} data set
#'
#' @description
#' A pre-calculated look up table (made with \code{farver::compare_colour}) based on pair-wise CIE2000 comparison of LAB colors for all whole value/chroma "chips" in the \code{munsell} data set.
#'
#' "Equivalent" chips table are based (fairly arbitrarily) on the 0.1% percentile of CIE2000 distances within the 8467x8467 distance matrix.
#'
#' @usage data(munequivalent)
#'
#' @references
#'  Thomas Lin Pedersen, Berendea Nicolae and Romain François (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
#'
#' @aliases munequivalent
#'
#' @keywords datasets
#'
#' @format A named list with 8467 elements, each containing a numeric vector of indices corresponding to the \code{munsell} data set, which has 8467 rows (unique, whole-number chips). Names have the format \code{HUE VALUE/CHROMA}, eg. \code{"7.5YR 4/4"}
"munequivalent"

#' .makeEquivalentMunsellLUT
#'
#' @description Makes the look up table based on pair-wise CIE2000 comparison of LAB colors for all whole value/chroma "chips" in the \code{munsell} data set via \code{farver::compare_colour}. Specify a threshold in terms of a probability level of CIE2000 distance (relative to whole dataset).
#'
#' @param threshold Quantile cutoff for "equivalent" colors, default \code{0.001}
#'
#' @references
#'  Thomas Lin Pedersen, Berendea Nicolae and Romain François (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
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
  load(system.file("data/munsell.rda", package="aqp")[1])

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
  system.time(x <- farver::compare_colour(from = munsell[,c('L','A','B')], from_space = 'lab',
                              to = munsell[,c('L','A','B')], to_space = 'lab',
                              method = 'cie2000', white_from = 'D65', white_to = 'D65'))
  xdat <- x
  x[lower.tri(x, diag = TRUE)] <- NA # remove lower triangle for statistics (only count each pair distance 1x)

  # calculate quantiles
  xqtl <- quantile(x, p = threshold, na.rm = TRUE)[1]

  # storing just the chips that fall within the closest e.g. tenth of a percentile: ~1.4MB in memory as list
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
#''
#' @description Uses a pre-calculated look up table (via \code{farver::compare_colour}) based on pair-wise CIE2000 comparison of LAB colors for all whole value/chroma "chips" in the \code{munsell} data set. The "equivalent" chips stored in the look up table are based (fairly arbitrarily) on the 0.1% percentile of CIE2000 distances within the 8467x8467 distance matrix.
#'
#' @param hue A character vector containing Munsell hues
#' @param value A numeric vector containing Munsell values (integer only)
#' @param chroma A numeric vector containing Munsell chromas (integer only)
#'
#' @return A named list; Each list element contains a data.frame with one or more rows of "equivalent" Munsell, RGB and LAB color coordinates from \code{munsell} data set.
#'
#' @references
#'  Thomas Lin Pedersen, Berendea Nicolae and Romain François (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
#'
#' @export
#'
#' @examples
#'
#' # 7.5YR 4/4 (the one and only)
#' equivalentMunsellChips("7.5YR", 4, 4)
#'
#' # 7.5YR 1/1
#' equivalentMunsellChips("7.5YR", 1, 1)
#'
#' # 10YR 6/8
#' equivalentMunsellChips("10YR", 6, 8)
#'
#' # compare visually a very red color
#' veryred <- equivalentMunsellChips("10R", 6, 28)[[1]]
#'
#' par(mar=c(0,0,1,1))
#'
#' pie(rep(1, nrow(veryred)), col = munsell2rgb(veryred$hue,
#'                                              veryred$value,
#'                                              veryred$chroma),
#'                            label = with(veryred, sprintf("%s %s/%s", hue, value, chroma)))
#'
#' table(veryred$hue) # 2 hues
#' table(veryred$value) # 2 values
#' table(veryred$chroma) # 10 chromas
#'
equivalentMunsellChips <- function(hue = NULL, value = NULL, chroma = NULL) {

  munequivalent <- NULL
  load(system.file("data/equivalent_munsell.rda", package="aqp")[1])

  munsell <- NULL
  load(system.file("data/munsell.rda", package="aqp")[1])

  chipdata <- data.frame(.id = 1:pmax(length(hue), length(value), length(chroma)),
                         hue = hue, value = value, chroma = chroma)

  mychips <- split(chipdata, f = chipdata$.id)

  lidx <- lapply(chipdata$.id, function(x) {
    # TODO: handle half values? e.g. 7.5YR 2.5/2?
    which(munsell$hue == chipdata$hue[x] &
            munsell$value == chipdata$value[x] &
            munsell$chroma == chipdata$chroma[x])
  })
  res <- lapply(lidx, function(i) munsell[munequivalent[i][[1]],])
  names(res) <- sprintf("%s %s/%s", hue, value, chroma)
  return(res)
}

