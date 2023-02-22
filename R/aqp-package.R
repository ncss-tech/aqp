#' Algorithms for Quantitative Pedology
#'
#' The aqp (Algorithms for Quantitative Pedology) package for R was developed
#' to address some of the difficulties associated with processing soils
#' information, specifically related to visualization, aggregation, and
#' classification of soil profile data. This package is based on a mix of S3/S4
#' functions and classes, and most functions use basic dataframes as input,
#' where rows represent soil horizons and columns define properties of those
#' horizons. Common to most functions are the requirements that horizon
#' boundaries are defined as depth from 0, and that profiles are uniquely
#' defined by an id column. The aqp package defines an S4 class,
#' "SoilProfileCollection", for storage of profile-level metadata, as well as
#' summary, print, and plotting methods that have been customized for common
#' tasks related to soils data.
#'
#' Demos: \code{demo(aqp)}
#'
#'
#' [Project homepage](http://ncss-tech.github.io/AQP/)
#'
#'
#' @name aqp-package
#' @aliases aqp-package aqp aqp.env
#' @docType package
#' @author Dylan E. Beaudette <debeaudette@@ucdavis.edu>, Pierre Roudier, Andrew G. Brown
#' @seealso \link{ca630}, \code{\link{sp1}, \link{sp2}, \link{sp3}, \link{sp4}, \link{sp5}}
#' @keywords package
#' @import data.table
#' @importFrom grDevices chull col2rgb colorRamp colorRampPalette colors convertColor grey hsv rgb rgb2hsv
#' @importFrom graphics abline arrows axis box grid image legend lines mtext par points polygon rect segments strheight strwidth text
#' @importFrom methods setClass setOldClass representation prototype new isGeneric setGeneric setReplaceMethod setMethod .hasSlot as new slot slot<- slotNames
#' @importFrom stats TukeyHSD aggregate aov approxfun as.dist as.formula cmdscale complete.cases dist formula median model.frame na.omit na.pass quantile rnorm runif sd splinefun terms update weighted.mean
#' @importFrom utils object.size packageVersion
#' @importFrom cluster pam daisy silhouette 
#' @importFrom grid grid.text gpar unit
#' @importFrom graphics plot
#' @importFrom lattice levelplot xyplot panel.abline panel.grid panel.lines panel.points panel.polygon panel.rect panel.segments panel.text strip.custom trellis.par.get
NULL

#' @export aqp.env
#' @noRd
"aqp.env"
