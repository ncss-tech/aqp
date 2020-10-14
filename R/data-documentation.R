##
## document data here
## don't forget: @usage data(XXX)


#' @title Pantone Colors / Munsell Lookup Table
#' 
#' @description A simple lookup table to convert \href{https://en.wikipedia.org/wiki/Pantone}{Pantone spot colors} into Munsell notation. Association is based on the "closest" Munsell color via \href{https://en.wikipedia.org/wiki/Color_difference#CIEDE2000}{CIE2000 distance metric (dE00)}. This is an experimental association between the two color systems and should not be used for precision color matching or mixing applications.
#' 
#' Possible uses include rough estimation of soil colors in the field, by means of color swatches based on the Pantone system. This type of color matching is most appropriate in an educational setting where official soil color books may be too expensive.
#' 
#' @keywords datasets
#' 
#' @usage data(pms.munsell.lut)
#' 
#' @format 
#' \describe{
#'   \item{code}{Pantone spot color code}
#'   \item{hex}{hex representation of sRGB colorspace, suitable for on-screen use}
#'   \item{munsell}{Munsell notation of closest color "chip"}
#'   \item{dE00}{delta-E 2000 metic describing the (perceptual) distance to the closest Munsell chip}
#' }
#' 
#' @references Data were sourced from:
#' \itemize{
#' \item{coated colors: }{\url{https://raw.githubusercontent.com/ajesma/Pantoner/gh-pages/csv/pantone-coated.csv}}
#' \item{uncoated colors: }{\url{https://github.com/ajesma/Pantoner/raw/gh-pages/csv/pantone-uncoated.csv}}
#' }
#' 
#' @details For now, lookup is performed by manual subset (see examples 1 and 2 below) or implicit subsetting by way of a join (example 3). Reverse lookup (Munsell -> Pantone) will not always result in a matching color, see example 3 below.
#' 
#' @note The lookup table contains entires for both coated and uncoated colors, these are indentified by a '-c' or '-u' suffix. For example, Pantone code '100-c' is associated with '10Y 9/9'.
#'
#'
#' Several Munsell chips are matched by multiple Pantone spot colors, e.g. 5YR 5/5.
#'  
#' 1    2    3    4    5    6    8    9 
#' 0.65 0.24 0.08 0.02 0.01 0.00 0.00 0.00 
#' 
#' @examples
#' 
#' # load LUT
#' data(pms.munsell.lut)
#' 
#' ## 1. Pantone -> Munsell
#' 
#' # colors to match
#' colors <- c('10YR 3/3', '7.5YR 4/6')
#' 
#' # index / subset match
#' idx <- pms.munsell.lut$munsell %in% colors
#' m <- pms.munsell.lut[idx, ]
#' 
#' # simple display
#' colorContrastPlot(m1 = m$munsell[1], m2 = m$munsell[2], labels = m$code)
#' 
#' ## 2. Munsell -> Pantone
#' colors <- c('723-c', '451-c')
#' 
#' # index / subset match
#' idx <- pms.munsell.lut$code %in% colors
#' m <- pms.munsell.lut[idx, ]
#' 
#' # simple display
#' colorContrastPlot(m1 = m$munsell[1], m2 = m$munsell[2], labels = m$code)

#' ## 3. integration with SPC
#' data(pms.munsell.lut)
#' data(sp6)
#' depths(sp6) <- id ~ top + bottom
#' 
#' # get the closest Munsell chip from color meter data
#' sp6$munsell <- getClosestMunsellChip(sp6$color, convertColors = FALSE)
#' 
#' # prepare an intermediate data.frame for performing join to LUT
#' h <- horizons(sp6)[, c(hzidname(sp6), 'munsell')]
#' 
#' # left join
#' # not all Munsell colors have a paired Pantone color
#' m <- merge(h, pms.munsell.lut, by = 'munsell', all.x = TRUE, sort = FALSE)
#' 
#' # splice into original SPC
#' horizons(sp6) <- m
#' 
#' # graphical check
#' par(mar = c(0, 0, 2, 1))
#' plotSPC(sp6, color = 'hex')
#' 
#' 
#' ## 4. multiple Pantone colors matching a single Munsell color
#' # 
#' colors <- pms.munsell.lut[pms.munsell.lut$munsell == '5YR 5/5', ]
#' colors <- colors[order(colors$dE00), ]
#' 
#' par(mar = c(0, 0, 2, 0), fg = 'white', bg = 'black')
#' soilPalette(colors$hex, lab = colors$code)
#' title('Pantone Colors Roughly Matching 5YR 5/5', col.main = 'white', line = 0)
#' 
"pms.munsell.lut"


#'
#' @title US State Soils
#' @description A listing of the 52 US state soils, including Puerto Rico and Virgin Islands.
#' @keywords datasets
#' @usage data(us.state.soils)
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
#' The original database "SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt" was provided by \href{http://www.munsellcolourscienceforpainters.com}{Paul Centore} and downloaded July, 2020. Reflectance values for odd chroma have been interpolated from adjacent chips. See \code{aqp/misc/utils/Munsell/} for the entire set of processing steps.
#' 
#' The original database contains the following description:
#' 
#' This file contains spectral reflectance measurements of X-Rite's 2007 Munsell Book of Color (Glossy Finish).  The measurements were made in 2012 with a ColorMunki spectrophotometer.  The first column is the Munsell name.  The remaining columns give reflectance values for 380 nm to 730 nm, in steps of 10 nm.  The reflectance is a value between 0 (indicating that no light at that wavelength is reflected) and 1 (indicating that all the light at that wavelength is reflected).  Occasionally an entry is slightly greater than 1.  The likely cause is random variability, and those entries can be adjusted to 1 with negligible loss. In all, 1485 colour samples were measured.  Researchers are invited to analyze the data in this file.
#' 
#' @usage data(munsell.spectra)
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
