
## TODO:
# * introduce a `jitter` argument to account for the possibility of varying geometry / class


#' @title Convert Horizon Boundary Distinctness to Vertical Offset
#' 
#' @description This function will convert USDA-NCSS horizon boundary distinctness codes into vertical (+/-) offsets in cm, based on the \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}.
#' 
#' @author D.E. Beaudette
#' 
#' @param x vector of boundary distinctness codes to be converted
#' 
#' @param codes code values, adjust as needed
#' 
#' @param offset vertical offset factors (cm), approximating 1/2 of the transitional zone thickness, see details
#' 
#' @return vector of offsets with same length as \code{x}
#' 
#' @details The default offsets are based on the high-end of ranges presented in "transitional zone thickness criteria" from the Field Book version 3.0 (page 2-6). Offsets are returned as 1/2 of the transitional zone thickness so that horizon boundaries can be adjusted up/down from horizon depths. See \code{\link{plotSPC}}, specifically the \code{hz.distinctness.offset} argument for visualization ideas. Missing data in \code{x} (NA) or codes that are not defined in \code{codes} are returned as 0 offsets.
#' 
#' Additional examples are available in the \href{https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html}{Visualization of Horizon Boundaries tutorial}.
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords manip
#' 
#' @references \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
#' 
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
  codes =  c('V', 'A', 'C', 'G', 'D'), 
  offset = c(0.5, 2, 5, 15, 20) / 2
) {	
  # no factors here
  x <- as.character(x)
  
  # lookup codes
  x.code <- match(x, codes)
  
  # lookup offsets
  x.offset <- offset[x.code]
  
  # NA -> 0
  x.offset <- ifelse(is.na(x.offset), 0, x.offset)
  
  return(x.offset)
}



#' @title Convert Horizon Boundary Topography to Line Type
#' 
#' @description This function will convert USDA-NCSS horizon boundary topography codes into line types, based on the \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}.
#' 
#' @author D.E. Beaudette
#' 
#' @details Visualization of horizon boundary topography can be difficult, line type offers an additional visual cue. See \code{hzTopographyCodeToOffset} for an offset-based approach. Additional examples are available in the \href{https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html}{Visualization of Horizon Boundaries tutorial}.
#' 
#' @param x vector of boundary topography codes to be converted
#' 
#' @param codes code values, adjust as needed
#' 
#' @param lty line types
#' 
#' @return vector of line types with same length as \code{x}
#' 
#' @seealso \code{\link{plotSPC}, \link{hzTopographyCodeToOffset}}
#' 
#' @keywords manip
#' 
#' @references \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
hzTopographyCodeToLineType <- function(
  x, 
  codes =  c('S', 'W', 'I', 'B'), 
  lty = c(1, 2, 3, 4)
) {	
  # no factors here
  x <- as.character(x)
  
  # lookup codes
  x.code <- match(x, codes)
  
  # lookup line types
  x.lty <- lty[x.code]
  
  # NA -> line type 1
  x.lty <- ifelse(is.na(x.lty), 1, x.lty)
  
  return(x.lty)
}


#' @title Convert Horizon Boundary Topography to Vertical Offset
#' 
#' @description This function will convert USDA-NCSS horizon boundary topography codes into a vertical offset, suitable for use in \code{plotSPC}. Default values are reasonable starting points for encoding smooth, wavy, irregular, or broken style horizon boundary topography as defined in \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}.
#' 
#' @details Additional examples are available in the \href{https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html}{Visualization of Horizon Boundaries tutorial}.
#' 
#' @author D.E. Beaudette
#' 
#' @param x vector of boundary topography codes to be converted
#' 
#' @param codes code values, adjust as needed
#' 
#' @param offset vertical offset (depth units) used to create "chevron" effect
#' 
#' @return vector of vertical offsets with same length as \code{x}
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords manip
#' 
#' @references \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
hzTopographyCodeToOffset <- function(
  x, 
  codes =  c('S', 'W', 'I', 'B'), 
  offset = c(0, 4, 8, 12)
) {	
  # no factors here
  x <- as.character(x)
  
  # lookup codes
  x.code <- match(x, codes)
  
  # lookup line types
  x.offset <- offset[x.code]
  
  # NA -> line type 1
  x.offset <- ifelse(is.na(x.offset), 0, x.offset)
  
  return(x.offset)
}

