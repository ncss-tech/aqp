# compute metrics of color contrast: delta-Hue, Value, Chroma and delta-E00
# m1: vector of Munsell colors ('10YR 3/3')
# m2: vector of Munsell colors ('10YR 3/4')

#' @title Metrics of Contrast Suitable for Comparing Soil Colors
#'
#' @description Pair-wise comparisons of Munsell color specifications, based on the NCSS color contrast classes (Soil Survey Technical Note 2) and CIE delta-E 2000 metric.
#'
#' @param m1 vector of Munsell colors ('10YR 3/3')
#'
#' @param m2 vector of Munsell colors ('10YR 3/6')
#'
#' @details This function is fully vectorized but expects input to be of the same length. Use `expand.grid()` to generate suitable input from 1:many or many:1 type comparisons. See \href{http://ncss-tech.github.io/AQP/aqp/color-contrast.html}{this tutorial} for an expanded discussion and more examples. Neutral colors are not mentioned in SSTN2: in this function any comparison to a neutral color (e.g. 'N 3/') are assigned a delta-hue of 1. Since SSTN2 expects hues to be counted clock wise from 5R, it possible to get very large delta-hue values for otherwise adjacent colors: '5R' vs. '2.5R'. This will be addressed in an update to the standards.
#' 
#' The most meaningful representation of color contrast is the CIE2000 (dE00) metric.
#'
#' @return
#' A `data.frame` with the following columns:
#'
#' \itemize{
#' \item{m1: }{Munsell color 1}
#' \item{m2: }{Munsell color 2}
#' \item{dH: }{delta-hue, as computed by \code{huePosition}}
#' \item{dV: }{delta-value, absolute value of difference in Munsell value (m1 vs. m2)}
#' \item{dc: }{delta-chroma, absolute value of difference in Munsell chroma (m1 vs. m2)}
#' \item{dE00: }{delta-E00, e.g. the [CIE delta-E as refined in 2000](https://en.wikipedia.org/wiki/Color_difference#CIEDE2000)}
#' \item{cc: }{soil color contrast class, as specified in Soil Survey Technical Note 2 [wayback machine URL](https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569)}
#' }
#'
#' @references
#'
#'  1. https://en.wikipedia.org/wiki/Color_difference
#'
#'  2. Soil Survey Technical Note 2 [wayback machine URL](https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569)
#'
#' @author D.E. Beaudette
#'
#' @note delta-E00 is computed by the \href{https://CRAN.R-project.org/package=farver}{farver package}.
#'
#' @seealso [colorContrastPlot], [huePosition], [huePositionCircle]
#'
#' @keywords manip
#' @export
#' @examples
#'
#' # two sets of colors to compare
#' m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', '7.5YR 3/4')
#' m2 <- c('5YR 3/4', '7.5YR 4/4', '2.5YR 2/2', '7.5YR 6/3')
#'
#' # contrast metrics
#' colorContrast(m1, m2)
#'
#' # adjacent chips
#' colorContrast('10YR 3/3', '10YR 3/4')
#' colorContrast('10YR 3/3', '7.5YR 3/3')
#'
#' # highly contrasting colors
#' # http://colour.granjow.net/fabercastell-polychromos.html
#' colorContrastPlot('10B 4/13', '10YR 10/15',
#' labels = c('helioblue-reddish', 'light cadmium yellow')
#' )
#' 
#' 
#' ## Note: neutral hues aren't defined in TN2
#' # approximation / extension of the concept
#' colorContrast(m1 = 'N 3/', m2 = 'N 6/')
#' 
#' #
#' colorContrast(m1 = '10YR 3/3', m2 = 'N 3/')
#' 
#' m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', 'N 3/')
#' m2 <- c('5YR 3/4', '7.5YR 4/4', '2.5YR 2/2', '7.5YR 6/3')
#' colorContrast(m1, m2)
#' 
colorContrast <- function(m1, m2) {

  # sanity check, need this for color distance eval
  if(!requireNamespace('farver', quietly = TRUE))
    stop('please install the `farver` package.', call.=FALSE)

  # sanity check: length of colors to compare should be equal
  if(length(m1) != length(m2)) {
    stop('inputs must be the same length', call. = FALSE)
  }

  # in case colors are encoded as factors
  m1 <- as.character(m1)
  m2 <- as.character(m2)

  # split into data.frame of hue/value/chroma
  m1.pieces <- parseMunsell(m1, convertColors = FALSE)
  m2.pieces <- parseMunsell(m2, convertColors = FALSE)

  # convert to value and chroma to numeric
  m1.pieces[[2]] <- as.numeric(m1.pieces[[2]])
  m1.pieces[[3]] <- as.numeric(m1.pieces[[3]])
  m2.pieces[[2]] <- as.numeric(m2.pieces[[2]])
  m2.pieces[[3]] <- as.numeric(m2.pieces[[3]])

  ## TODO: contrast with neutral hues are currently undefined
  ## TODO: incorporate new huePosition features for locating angular proximity CW or CCW
  ## TODO: add a notes / flag field in the results
  
  # difference in number of hue chips, clock-wise, as specified in:
  # Soil Survey Technical Note 2 [wayback machine URL](https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569)
  # including neutral hues in position 1
  dH <- abs(huePosition(m1.pieces[[1]], includeNeutral = TRUE) - huePosition(m2.pieces[[1]], includeNeutral = TRUE))
  
  ## TODO: not in standards
  # correction for neutral hues, enforce with XOR
  #   no N: dH is left alone
  #   single N: dH is always 1
  #   double N: dH is 0
  N.idx <- which(xor(m1.pieces[[1]] == 'N', m2.pieces[[1]] == 'N'))
  
  if(length(N.idx) > 0) {
    dH[N.idx] <- 1
  }
  
  
  # difference in number of value chips
  dV <- abs(m1.pieces[[2]] - m2.pieces[[2]])

    # difference in number of chroma chips
  dC <- abs(m1.pieces[[3]] - m2.pieces[[3]])

  # get CIE LAB representation
  m1.lab <- parseMunsell(m1, convertColors = TRUE, returnLAB=TRUE)
  m2.lab <- parseMunsell(m2, convertColors = TRUE, returnLAB=TRUE)


  ## TODO: there is likely room for improvement here:
  #         * parallel evaluation when length(m1) > 1000 (?) (simple via furrr)
  #         * vectorized call to compare_colour() -> reshaping of result

  # delta E00
  #
  # we don't need the full distance matrix,
  # iterate over rows, likely scales better
  d <- list()
  for(i in 1:nrow(m1.lab)){
    d[i] <- farver::compare_colour(m1.lab[i, ], m2.lab[i, ], from_space='lab', method = 'CIE2000', white_from = 'D65')
  }
  dE00 <- unlist(d)

  # NCSS color contrast classes
  # Soil Survey Technical Note 2 [wayback machine URL](https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569)
  #
  # value1, chroma1, value2, chroma2, dH, dV, dC
  cc <- contrastClass(m1.pieces[[2]], m1.pieces[[3]], m2.pieces[[2]], m2.pieces[[3]], dH, dV, dC)

  # combine into DF and return
  res <- data.frame(m1, m2, dH, dV, dC, dE00, cc, stringsAsFactors = FALSE)
  return(res)
}



