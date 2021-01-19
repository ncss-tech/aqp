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
#'   \item{dE00}{delta-E 2000 metric describing the (perceptual) distance to the closest Munsell chip}
#' }
#' 
#' @references Data were sourced from:
#' \itemize{
#' \item{coated colors: }{\url{https://raw.githubusercontent.com/ajesma/Pantoner/gh-pages/csv/pantone-coated.csv}}
#' \item{uncoated colors: }{\url{https://github.com/ajesma/Pantoner/raw/gh-pages/csv/pantone-uncoated.csv}}
#' }
#' 
#' @details For now, lookup is performed by manual subset (see examples 1 and 2 below) or implicit subset by way of a join (example 3). Reverse lookup (Munsell -> Pantone) will not always result in a matching color, see example 3 below.
#' 
#' @note The lookup table contains entries for both coated and un-coated colors, these are identified by a '-c' or '-u' suffix. For example, Pantone code '100-c' is associated with '10Y 9/9'.
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
#' @description A listing of the 50 US state soils, along with Puerto Rico and Virgin Islands.
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
#' The original database "SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt" was provided by Paul Centore and downloaded July, 2020. Reflectance values for odd chroma have been interpolated from adjacent chips. See \code{aqp/misc/utils/Munsell/} for the entire set of processing steps.
#' 
#' Munsell value typically ranges from 2-9, and chroma from 1-12. Ranges vary by hue. Run \code{aqp:::.summarizeMunsellSpectraRanges()} for a detailed listing by hue.
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
#' @usage data(equivalent_munsell)
#' 
#' @aliases equivalent_munsell
#'
#' @seealso \code{\link{equivalentMunsellChips}}
#' 
#' @references
#' Gaurav Sharma, Wencheng Wu, Edul N. Dalal. (2005). The CIEDE2000 Color-Difference Formula: Implementation Notes, Supplementary Test Data, and Mathematical Observations. COLOR research and application. 30(1):21-30. http://www2.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf
#' 
#'  Thomas Lin Pedersen, Berendea Nicolae and Romain Francois (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
#'  
#' Dong, C.E., Webb, J.B., Bottrell, M.C., Saginor, I., Lee, B.D. and Stern, L.A. (2020). Strengths, Limitations, and Recommendations for Instrumental Color Measurement in Forensic Soil Characterization. J Forensic Sci, 65: 438-449. https://doi.org/10.1111/1556-4029.14193
#'
#' @keywords datasets
#'
#' @format A named list with 8467 elements, each containing a numeric vector of indices corresponding to the \code{munsell} data set, which has 8467 rows (unique, whole-number chips). Names have the format \code{HUE VALUE/CHROMA}, eg. \code{"7.5YR 4/4"}
#' 
#' @examples 
#' data(equivalent_munsell)
"equivalent_munsell"



