##
## document data here
##

#'
#' @title US State Soils
#' @description A listing of the 52 US state soils, including Puerto Rico and Virgin Islands.
#' @keywords datasets
#' @format 
#' \describe{
#'   \item{state}{state name}
#'   \item{abbreviated}{abbreviated state name}
#'   \item{series}{soil series name}
#' }
#' 
#' 
#'
"us.state.soils"

#'
#' @title Spectral Library of Munsell Colors
#' 
#' @description 
#' 
#' The original database (\href{http://centore.isletech.net/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt}{SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt}) was provided by Paul Centore and downloaded September, 2020. Reflectance values for odd chroma have been interpolated from adjacent chips. See \code{aqp/misc/utils/Munsell/} for the entire set of processing steps.
#' 
#' The original database contains the following description:
#' 
#' This file contains spectral reflectance measurements of X-Rite's 2007 Munsell Book of Color (Glossy Finish).  The measurements were made in 2012 with a ColorMunki spectrophotometer.  The first column is the Munsell name.  The remaining columns give reflectance values for 380 nm to 730 nm, in steps of 10 nm.  The reflectance is a value between 0 (indicating that no light at that wavelength is reflected) and 1 (indicating that all the light at that wavelength is reflected).  Occasionally an entry is slightly greater than 1.  The likely cause is random variability, and those entries can be adjusted to 1 with negligible loss. In all, 1485 colour samples were measured.  Researchers are invited to analyze the data in this file.
#' 
#' @references 
#' Centore, Paul. Colour Tools for Painters. \url{http://www.munsellcolourscienceforpainters.com/}.
#' 
#' @aliases munsell.spectra.wide
#'
#' @keywords datasets
#'
#' @format A data frame with 89496 rows and 10 variables:
#' \describe{
#'   \item{munsell}{munsell color}
#'   \item{hue}{hue component}
#'   \item{value}{value component}
#'   \item{chroma}{chroma component}
#'   \item{wavelength}{wavelength (nm)}
#'   \item{reflectance}{reflectance}
#' }
"munsell.spectra"
