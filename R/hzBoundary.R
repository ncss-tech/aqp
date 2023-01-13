
## TODO:
# * introduce a `jitter` argument to account for the possibility of varying geometry / class


## helper function for matching either the full code, or first-letter abbreviation to a value
.matchCodeOrTerm <- function(x, codes, values) {
  # just in case single letter codes are used
  codes.single <- substr(codes, 1, 1)
  
  # no factors here
  x <- as.character(x)
  
  # convert to lower case
  x <- tolower(x)
  
  # index to single-letter codes vs. terms
  idx <- which(sapply(x, function(i) nchar(i) == 1))
  
  if(length(idx) > 0) {
    # look-up via single letter codes
    x.code.single <- match(x, codes.single)
    
    # look-up terms just in case
    x.code <- match(x, codes)
    
    # if any single letter code results in NA, attempt full term match
    x.code <- ifelse(is.na(x.code.single), x.code, x.code.single)
  } else {
    # look-up terms only
    x.code <- match(x, codes) 
  }
  
  # look-up associated value
  res <- values[x.code]
  
  return(res)
}



#' @title Convert Horizon Boundary Distinctness to Vertical Offset
#' 
#' @description This function will convert USDA-NCSS horizon boundary distinctness codes into vertical (+/-) offsets in cm, based on the \href{https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991}{Field Book for Describing and Sampling Soils, version 3.0}.
#' 
#' @author D.E. Beaudette
#' 
#' @param x vector of boundary distinctness codes to be converted
#' 
#' @param codes character vector of distinctness terms ('clear') or codes ('C'), case insensitive, see details
#' 
#' @param offset vertical offset factors (cm), approximating 1/2 of the transitional zone thickness, see details
#' 
#' @return vector of offsets with same length as \code{x}
#' 
#' @details The default offsets are based on the high-end of ranges presented in "transitional zone thickness criteria" from the Field Book version 3.0 (page 2-6). Offsets are returned as 1/2 of the transitional zone thickness so that horizon boundaries can be adjusted up/down from horizon depths. See \code{\link{plotSPC}}, specifically the \code{hz.distinctness.offset} argument for visualization ideas. Missing data in \code{x} (NA) or codes that are not defined in \code{codes} are returned as 0 offsets.
#' 
#' Either format (or mixture) are accepted, case insensitive:
#' 
#'   * terms: `c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')`
#'   * coded values: `c('v', 'a', 'c', 'g', d')`
#' 
#' Additional examples are available in the \href{https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html}{Visualization of Horizon Boundaries tutorial}.
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords manip
#' 
#' @references \href{https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
#' @export
#' @examples 
#' # example data
#' data(sp1)
#' 
#' # compute 1/2 transitional zone thickness from distinctness codes
#' sp1$hzdo <- hzDistinctnessCodeToOffset(sp1$bound_distinct)
#' 
#' # convert colors from Munsell to hex-encoded RGB
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#' 
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#' hzdesgnname(sp1) <- 'name'
#' 
#' # adjust margins
#' op <- par(mar=c(0,0,0,1.5))
#' 
#' # sketches, adjust width, adjust text size, include coded hz distinctness offsets
#' plotSPC(sp1, width=0.3, cex.names=0.75, hz.distinctness.offset = 'hzdo')
#' 
#' # clean-up
#' par(op)
hzDistinctnessCodeToOffset <- function(
  x, 
  codes =  c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse'), 
  offset = c(0.5, 2, 5, 15, 20) / 2
) {	
  
  # match term or first letter code
  res <- .matchCodeOrTerm(x, codes, offset)
  
  # interpret NA as 0
  res <- ifelse(is.na(res), 0, res)
  
  return(res)
}



#' @title Convert Horizon Boundary Topography to Line Type
#' 
#' @description This function will convert USDA-NCSS horizon boundary topography codes into line types, based on the \href{https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991}{Field Book for Describing and Sampling Soils, version 3.0}.
#' 
#' @author D.E. Beaudette
#' 
#' @details Visualization of horizon boundary topography can be difficult, line type offers an additional visual cue. See \code{hzTopographyCodeToOffset} for an offset-based approach. Additional examples are available in the \href{https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html}{Visualization of Horizon Boundaries tutorial}. Missing data in \code{x} (NA) or codes that are not defined in \code{codes} are returned as line type 1.
#' 
#' Either format (or mixture) are accepted, case insensitive:
#' 
#'   * terms: `c('smooth', 'wavy', 'irregular', 'broken')`
#'   * coded values: `c('s', 'w', 'i', 'b')`
#' 
#' @param x vector of boundary topography codes to be converted
#' 
#' @param codes character vector of topography terms ('smooth') or codes ('S'), case insensitive, see details
#' 
#' @param lty line types
#' 
#' @return vector of line types with same length as \code{x}
#' 
#' @seealso \code{\link{plotSPC}, \link{hzTopographyCodeToOffset}}
#' 
#' @keywords manip
#' 
#' @references \href{https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991}{Field Book for Describing and Sampling Soils, version 3.0}
#' @export
hzTopographyCodeToLineType <- function(
  x, 
  codes =  c('smooth', 'wavy', 'irregular', 'broken'), 
  lty = c(1, 2, 3, 4)
) {	
  
  # match term or first letter code
  res <- .matchCodeOrTerm(x, codes, lty)
  
  # interpret NA as 1
  res <- ifelse(is.na(res), 1, res)
  
  return(res)
}


#' @title Convert Horizon Boundary Topography to Vertical Offset
#' 
#' @description This function will convert USDA-NCSS horizon boundary topography codes into a vertical offset, suitable for use in \code{plotSPC}. Default values are reasonable starting points for encoding smooth, wavy, irregular, or broken style horizon boundary topography as defined in \href{https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991}{Field Book for Describing and Sampling Soils, version 3.0}.
#' 
#' @details Additional examples are available in the \href{https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html}{Visualization of Horizon Boundaries tutorial}. Missing data in \code{x} (NA) or codes that are not defined in \code{codes} are returned with an offset of 0.
#' 
#' Either format (or mixture) are accepted, case insensitive:
#' 
#'   * terms: `c('smooth', 'wavy', 'irregular', 'broken')`
#'   * coded values: `c('s', 'w', 'i', 'b')`
#' 
#' @author D.E. Beaudette
#' 
#' @param x vector of boundary topography codes to be converted
#' 
#' @param codes character vector of topography terms ('smooth') or codes ('S'), case insensitive, see details
#' 
#' @param offset vertical offset (depth units) used to create "chevron" effect
#' 
#' @return vector of vertical offsets with same length as \code{x}
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords manip
#' @export
#' @references \href{https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
hzTopographyCodeToOffset <- function(
  x, 
  codes =  c('smooth', 'wavy', 'irregular', 'broken'), 
  offset = c(0, 4, 8, 12)
) {	
  
  # match term or first letter code
  res <- .matchCodeOrTerm(x, codes, offset)
  
  # interpret NA as 0
  res <- ifelse(is.na(res), 0, res)
  
  return(res)
}

