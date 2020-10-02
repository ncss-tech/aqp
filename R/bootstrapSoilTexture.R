

#'
#' @title Bootstrap Soil Texture Data
#' 
#' @description Simulate realistic sand/silt/clay values (a composition) using the Dirichlet distribution. Parameters of the Dirichlet distribution are estimated using example data provided to the function.
#' 
#' @author D.E. Beaudette
#' 
#' @param ssc a \code{data.frame} object with 3 columns: `sand`, `silt`, `clay` and at least three rows of data within the range of 0-100 (percent). NA are automatically removed, but care should be taken to ensure that the sand/silt/clay values add to 100 percent. See details.
#' 
#' @param n number of simulated compositions. See details.
#' 
#' @return a \code{list} containing:
#' 
#' \itemize{
#'  \item{samples}{\code{data.frame} of simulated sand, silt, clay values}
#'  \item{mean}{compositional mean}
#'  \item{var}{compositional variance-covariance matrix}
#'  \item{D.alpha}{(fitted) alpha parameters of the Dirichlet distribution}
#' }
#' 
#' 
#' 
#' @references 
#' 
#' Aitchison, J. (1986) The Statistical Analysis of Compositional Data Monographs on Statistics and Applied Probability. Chapman & Hall Ltd., London (UK). 416p.
#' 
#' Aitchison, J, C. Barcel'o-Vidal, J.J. Egozcue, V. Pawlowsky-Glahn (2002) A consise guide to the algebraic geometric structure of the simplex, the sample space for compositional data analysis, Terra Nostra, Schriften der Alfred Wegener-Stiftung, 03/2003
#'
#' @examples
#' 
#' \donttest{
#' if(
#' requireNamespace("compositions") &
#'   requireNamespace("soiltexture")
#' ) {
#'   
#'   # sample data
#'   data('sp6')
#'   depths(sp6) <- id ~ top + bottom
#'   
#'   # I still like this
#'   ssc <- horizons(sp6)[grep('^Bt', sp6$name), c('sand', 'silt', 'clay')]
#'   names(ssc) <- toupper(names(ssc))
#'   
#'   # simulate 100 samples
#'   s <- bootstrapSoilTexture(ssc, n = 100)
#'   s <- s$samples
#'   
#'   # empty soil texture triangle
#'   TT <- soiltexture::TT.plot(
#'     class.sys= "USDA-NCSS.TT",
#'     main= "",
#'     tri.sum.tst=FALSE,
#'     cex.lab=0.75,
#'     cex.axis=0.75,
#'     frame.bg.col='white',
#'     class.lab.col='black',
#'     lwd.axis=1.5,
#'     arrows.show=TRUE,
#'     new.mar = c(3, 0, 0, 0)
#'   )
#'   
#'   # add original data points
#'   soiltexture::TT.points(
#'     tri.data = s, geo = TT, col='firebrick', 
#'     pch = 3, cex = 0.5, lwd = 1, 
#'     tri.sum.tst = FALSE
#'   )
#'   
#'   # add simulated points
#'   soiltexture::TT.points(
#'     tri.data = ssc, geo = TT, bg='royalblue', 
#'     pch = 22, cex = 1, lwd = 1, 
#'     tri.sum.tst = FALSE
#'   )
#'   
#'   # simple legend
#'   legend('top', 
#'          legend = c('Source', 'Simulated'), 
#'          pch = c(22, 3), 
#'          col = c('black', 'firebrick'), 
#'          pt.bg = c('royalblue', NA), 
#'          horiz = TRUE, bty = 'n'
#'   )
#'   
#'   
#' }
#' 
#' }
#' 
bootstrapSoilTexture <- function(ssc, n = 100) {
  
  if(!requireNamespace("compositions", quietly = TRUE))
    stop("package `compositions` is required", call.=FALSE)
  
  # filter NA
  ssc <- na.omit(ssc)
  
  # sanity check: should have 3 columns and > 3 rows
  if(ncol(ssc) < 3 | nrow(ssc) < 3) {
    stop('insufficient observations or incorrect column specification', call. = FALSE)
  }
  
  # sanity check: columns should be sand, silt, clay
  if(! all(tolower(names(ssc)) == c('sand', 'silt', 'clay'))) {
    stop('column names and ordering should follow: `sand`, `silt`, `clay`', call. = FALSE)
  }
  
  # sanity check: data should be in the range of 0-100
  range.check <- range(sapply(ssc, range))
  if(! all(range.check >= 0 & range.check < 100)) {
    stop('data should be in the range of 0 to 100 (%)', call. = FALSE)
  }
     
  # convert to a closed / proportional composition object
  # with max value of 100%
  z <- compositions::acomp(ssc, total = 100)
  
  # fit 3-term alpha parameters of Dirichlet distribution
  # note backflips required when not loading entire compositions package
  # the following is the expanded form of default arguments to fitDirichlet()
  el <- compositions::mean.rmult(compositions::ult(z), robust = FALSE)
  D <- compositions::fitDirichlet(z, elog = el)
  
  # safely compute compositional mean / variance-covariance
  mean.comp <- compositions::meanCol(z)
  var.comp <- compositions::var(z, robust = FALSE, method = 'pearson')
  
  # draw simulated values
  s <- compositions::rDirichlet.acomp(n = n, alpha = D$alpha)
  
  # convert back to format that is suitable for plotting on the TT
  s <- as.data.frame(unclass(s) * 100)
  names(s) <- names(ssc)
  
  # package results into a list
  res <- list(
    samples = s,
    mean = mean.comp,
    var = var.comp,
    D.alpha = D$alpha
  )
  
  return(res)
}
