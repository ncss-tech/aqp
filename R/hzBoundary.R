

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
#' @details The default offests are based on the high-end of ranges presented in "transitional zone thickness criteria" from the Field Book version 3.0 (page 2-6). Offsets are returned as 1/2 of the transitional zone thickness so that horizon boundaries can be adjusted up/down from horizon depths. See \code{\link{plotSPC}}, specifically the \code{hz.distinctness.offset} argument for vizualization ideas. Missing data in \code{x} (NA) or codes that are not defined in \code{codes} are returned as 0 offsets.
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords manip
#' 
#' @references \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
#' 
#' @examples 
#' data(sp1)
#' hzDistinctnessCodeToOffset(sp1$bound_distinct)
#' 

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



#' @title Convert Horizon Boundary Topography Distinctness to Line Type
#' 
#' @description This function will convert USDA-NCSS horizon boundary topography codes into line types, based on the \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}.
#' 
#' @author D.E. Beaudette
#' 
#' @param x vector of boundary topography codes to be converted
#' 
#' @param codes code values, adjust as needed
#' 
#' @param lty line types
#' 
#' @return vector of line types with same length as \code{x}
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords manip
#' 
#' @references \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/research/guide/?cid=nrcs142p2_054184}{Field Book for Describing and Sampling Soils, version 3.0}
#' 
#' 
#' @examples 
#' 
#' example data
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



