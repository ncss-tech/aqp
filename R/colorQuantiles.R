## TODO: `sigma` should be delta-E00
## https://github.com/ncss-tech/aqp/issues/67
# closest Munsell chip to LAB coordinates and error
.closestMunselltoCIELAB <- function(lab) {
  lab <- as.matrix(lab)
  srgb <- grDevices::convertColor(lab, from = 'Lab', to = 'sRGB', from.ref.white='D65', to.ref.white='D65', clip=FALSE)
  res <- rgb2munsell(srgb)
  res.txt <- sprintf("%s %s/%s\n(%.3f)", res$hue, res$value, res$chroma, res$sigma)
  return(res.txt)
}



## TODO: quantiles should be weighted by thickness, can we do this via slice()?
## TODO: finish documentation + links

#' @title Soil Color Range via Quantiles
#'
#' @param soilColors vector of R colors (sRGB colorspace)
#' @param p marginal quantiles of interest
#'
#' @description Estimate central tendency and spread of soil color using marginal quantiles and L1 median of CIELAB coordinates.
#' 
#' @details Colors are converted from sRGB to CIELAB (D65 illuminant), marginal quantiles of L,A,B coordinates are estimated, and L1 median {L,A,B} is estimates. The closest Munsell chips (via Musell/CIELAB lookup table provided by \code{munsell}) and R colors are determined by locating chips closest to the marginal quantiles and L1 median.
#' 
#' The results can be conveniently inspected using \code{plotColorQuantiles}.
#'
#' @note This is a work in progress.
#' 
#' @author D.E. Beaudette
#' 
#' @return A List containing the following elements:
#' 
#' \itemize{
#' \item{marginal: }{data.frame containing marginal quantiles in CIELAB (D65), closest colors and Munsell chips}
#' \item{L1: }{L1 median CIELAB (D65) values, closest color and Munsell chip}
#' }
#' 
#' @export
#'
#' @examples
#' 
#'\dontrun{
#' # example data, see manual page for details
#' data(sp5)
#' 
#' # slice top 25 cm
#' s <- slice(sp5, 1:25 ~ .)
#' 
#' # check some of the data
#' par(mar=c(0,0,0,0))
#' plot(sample(s, 25), divide.hz=FALSE, name='', print.id=FALSE, width=0.5)
#' 
#' # colors
#' previewColors(unique(s$soil_color))
#' 
#' # compute marginal quantiles and L1 median
#' cq <- colorQuantiles(s$soil_color)
#' 
#' # simple graphical display of results
#' plotColorQuantiles(cq)
#' }
#' 
colorQuantiles <- function(soilColors, p = c(0.05, 0.5, 0.95)) {

  # sanity check, need this for L1 median
  if(!requireNamespace('Gmedian'))
    stop('package `Gmedian` is required', call.=FALSE)

  # hex represntation -> sRGB
  soilColors.srgb <- t(col2rgb(soilColors)) / 255
  # sRGB -> CIE LAB
  soilColors.lab <- convertColor(soilColors.srgb, from = 'sRGB', to = 'Lab', from.ref.white='D65', to.ref.white='D65', clip=FALSE)
  # convert to DF for use in diana
  soilColors.lab <- as.data.frame(soilColors.lab)
  names(soilColors.lab) <- c('L', 'A', 'B')

  # marginal quantiles
  q.L <- quantile(soilColors.lab$L, probs = p, na.rm = TRUE)
  q.A <- quantile(soilColors.lab$A, probs = p, na.rm = TRUE)
  q.B <- quantile(soilColors.lab$B, probs = p, na.rm = TRUE)

  # find observed color with the closest L-coordinate, linear distances
  L.q.idx <- c(
    which.min(sqrt((q.L[1] - soilColors.lab$L) ^ 2)),
    which.min(sqrt((q.L[2] - soilColors.lab$L) ^ 2)),
    which.min(sqrt((q.L[3] - soilColors.lab$L) ^ 2))
  )

  # find observed color with the closest A-coordinate, linear distances
  A.q.idx <- c(
    which.min(sqrt((q.A[1] - soilColors.lab$A) ^ 2)),
    which.min(sqrt((q.A[2] - soilColors.lab$A) ^ 2)),
    which.min(sqrt((q.A[3] - soilColors.lab$A) ^ 2))
  )

  # find observed color with the closest B-coordinate, linear distances
  B.q.idx <- c(
    which.min(sqrt((q.B[1] - soilColors.lab$B) ^ 2)),
    which.min(sqrt((q.B[2] - soilColors.lab$B) ^ 2)),
    which.min(sqrt((q.B[3] - soilColors.lab$B) ^ 2))
  )

  # L1 median
  # close to but not actually an obs within original data... why?
  # this is sometimes quite distant from marginal medians... why?
  L1 <- Gmedian::Gmedian(soilColors.lab, nstart = 10)

  ## find closest Munsell chips via CIE LAB coordinates
  # this is the closest Munsell chip to the L1 median color
  L1.closest <- .closestMunselltoCIELAB(L1)

  # closest munsell chip to marginal L,A,B quantiles
  L.closest <- .closestMunselltoCIELAB(soilColors.lab[L.q.idx, ])
  A.closest <- .closestMunselltoCIELAB(soilColors.lab[A.q.idx, ])
  B.closest <- .closestMunselltoCIELAB(soilColors.lab[B.q.idx, ])


  ## find closest observed color to L1 median via CIE2000 distance metric
  ## requires farver >= 2.0.3
  if( !requireNamespace('farver', quietly = TRUE) | packageVersion("farver") < '2.0.3' ) {
    message('CIE2000 comparisons require `farver` version 2.0.3 or greater, using Euclidean distance in CIELAB instead', call.=FALSE)
    d <- farver::compare_colour(from=L1, to=soilColors.lab, from_space='lab', method = 'cie2000')
  } else {
    # backup plan using Euclidean distance in CIELAB
    d <- sqrt(rowSums(sweep(soilColors.lab, MARGIN = 2, STATS=L1, FUN = '-')^2))
  }


  # assign L1 color
  L1.color <- soilColors[which.min(d)]

  # closest observed colors to marginal quantiles
  q.L.colors <- soilColors[L.q.idx]
  q.A.colors <- soilColors[A.q.idx]
  q.B.colors <- soilColors[B.q.idx]

  # marginal quantiles
  q.L.values <- soilColors.lab$L[L.q.idx]
  q.A.values <- soilColors.lab$A[A.q.idx]
  q.B.values <- soilColors.lab$B[B.q.idx]


  # combine into single DF for plotting
  res <- list(
    marginal=data.frame(
      p=p,
      L=q.L.values,
      A=q.A.values,
      B=q.B.values,
      L_colors=q.L.colors,
      A_colors=q.A.colors,
      B_colors=q.B.colors,
      L_chip=L.closest,
      A_chip=A.closest,
      B_chip=B.closest,
      stringsAsFactors = FALSE
    ),
    L1=data.frame(
      p=0.5,
      L=L1[, 1],
      A=L1[, 2],
      B=L1[, 3],
      L1_color=L1.color,
      L1_chip=L1.closest,
      stringsAsFactors = FALSE
    )
  )

  return(res)

}


#' @title Visualize Color Quantiles
#' 
#' @description This function creates a visualization of the ouptut from \code{colorQuantiles}.
#' 
#' @param res list returned by \code{colorQuantiles}
#' @param pt.cex scaling factor for color chips
#' @param title optional title printed above color quantile figure
#' @param mar customized margins, see details
#'
#' @author D.E. Beaudette
#'
#' @export
#' 
#' 
plotColorQuantiles <- function(res, pt.cex=7, title='', mar=c(2,1.5,1,1)) {
  par(mar=mar, mfrow=c(3,1))

  # vertical spacing
  m.y <- 2
  L1.y <- 1
  y.lim <- c(0.5, 2.5)

  # pre-make axis
  L.axis <- pretty(zapsmall(res$marginal$L), n = 10)
  A.axis <- pretty(zapsmall(res$marginal$A), n = 10)
  B.axis <- pretty(zapsmall(res$marginal$B), n = 10)

  ## L coordinates
  plot(res$marginal$L, rep(m.y, times=3), pch=22, bg=res$marginal$L_colors, cex=pt.cex, xlim=range(L.axis), ylim=y.lim, axes=FALSE, xlab='', ylab='', main=title, col.main=par('fg'))
  points(res$L1$L, L1.y, pch=22, bg=res$L1$L1_color, cex=pt.cex)
  text(res$marginal$L, rep(m.y, times=3), labels = res$marginal$p, pos = 3, offset = 1.5)
  text(res$L1$L, L1.y, labels = 'L1', pos = 3, offset = 1.5)
  text(res$marginal$L, rep(m.y, times=3), res$marginal$L_chip, pos=1, offset = 1.5)
  text(res$L1$L, L1.y, labels = res$L1$L1_chip, pos = 2, offset = 1.5)
  axis(1, line = -2, at = L.axis, col.axis=par('fg'))
  mtext('L', side = 2, line=-0.125, font=2, las=1)
  # title('CIELAB Color Space', cex=1.5, line=-1)

  ## A coordinates
  plot(res$marginal$A, rep(m.y, times=3), pch=22, bg=res$marginal$A_colors, cex=pt.cex, xlim=range(A.axis), ylim=y.lim, axes=FALSE, xlab='', ylab='')
  points(res$L1$A, L1.y, pch=22, bg=res$L1$L1_color, cex=pt.cex)
  text(res$marginal$A, rep(m.y, times=3), labels = res$marginal$p, pos = 3, offset = 1.5)
  text(res$L1$A, L1.y, labels = 'L1', pos = 3, offset = 1.5)
  text(res$marginal$A, rep(m.y, times=3), res$marginal$A_chip, pos=1, offset = 1.5)
  text(res$L1$A, L1.y, labels = res$L1$L1_chip, pos = 2, offset = 1.5)
  axis(1, line = -2, at=A.axis, col.axis=par('fg'))
  mtext('A', side = 2, line=-0.125, font=2, las=1)

  plot(res$marginal$B, rep(m.y, times=3), pch=22, bg=res$marginal$B_colors, cex=pt.cex, xlim=range(B.axis), ylim=y.lim, axes=FALSE, xlab='', ylab='')
  points(res$L1$B, L1.y, pch=22, bg=res$L1$L1_color, cex=pt.cex)
  text(res$marginal$B, rep(m.y, times=3), labels = res$marginal$p, pos = 3, offset = 1.5)
  text(res$L1$B, L1.y, labels = 'L1', pos = 3, offset = 1.5)
  text(res$marginal$B, rep(m.y, times=3), res$marginal$B_chip, pos=1, offset = 1.5)
  text(res$L1$B, L1.y, labels = res$L1$L1_chip, pos = 2, offset = 1.5)
  axis(1, line = -2, at=B.axis, col.axis=par('fg'))
  mtext('B', side = 2, line=-0.125, font=2, las=1)


}

