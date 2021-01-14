
#' @title Estimate Soil Depth
#' @description Estimate the soil depth of a single profile within a SoilProfileCollection object. This function would typically be called by \code{\link{profileApply}}.
#' 
#' @param f SoilProfileCollection object of length 1, e.g. a single profile
#' @param name name of the column that contains horizon designations
#' @param p REGEX pattern for determining "contact", or depth to some morphologic feature (e.g. `Bt`)
#' @param selection function applied in the presence of multiple matching horizons: \code{min} (default), \code{max}, \code{mean}, etc.
#' @param no.contact.depth in the absence of contact matching \code{p}, a depth at which we can assume a standard depth-to-contact
#' @param no.contact.assigned value assigned when no contact is encountered at or below \code{no.contact.depth}
#' 
#' @return single value representing the depth to `contact` or \code{no.contact.assigned}
#' 
#' @details The choice of a \code{selection} function usually follows:
#' 
#' \code{min}: the top of the first matching horizon, \code{max}: the top bot the last matching horizon, 
#' or possibly \code{mean}: somewhere in-between.
#' 
#' 
#' @author D.E. Beaudette and J.M. Skovlin
#' 
#' @seealso \code{\link{getSoilDepthClass}}, \code{\link{profileApply}}
#' 
#' @keywords manip
#' 
#' @examples 
#' 
#' 
#' ## consider a situation where there were multiple candidate
#' ## "contacts": 2 Cd horizons over an R
#' 
#' # init hypothetical profile
#' d <- data.frame(
#'   id = '1',
#'   top = c(0, 10, 20, 30, 40, 50, 60),
#'   bottom = c(10, 20, 30, 40, 50, 60, 80),
#'   name = c('A', 'Bt1', 'Bt2', 'BC', 'Cd1', 'Cd2', 'R'),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # upgrade to SPC
#' depths(d) <- id ~ top + bottom
#' 
#' # visual check
#' par(mar = c(0, 0, 0, 1))
#' plotSPC(d, hz.depths = TRUE, name.style = 'center-center', cex.names = 1)
#' 
#' # top of the first Cd
#' estimateSoilDepth(d, name = 'name')
#' 
#' # top of the first Cd
#' estimateSoilDepth(d, name = 'name', selection = min)
#' 
#' # top of the R
#' estimateSoilDepth(d, name = 'name', selection = max)
#' 
#' # top of the second Cd
#' estimateSoilDepth(d, name = 'name', selection = max, p = 'Cd')
#'
#'
#' ## another example
#' 
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#' 
#' # apply to each profile in a collection, and save as site-level attribute
#' sp1$depth <- profileApply(sp1, estimateSoilDepth, name='name')
#' 
#' # this function can be used to "find" depth to any feature 
#' # that can be defined via REGEX pattern matching on the horizon name
#' # for example, locate the depth to the top "Bt" horizon
#' # returning NA when there is no match
#' sp1$top_Bt <- profileApply(
#'   sp1, estimateSoilDepth, 
#'   name='name', 
#'   p='Bt', 
#'   no.contact.depth=0, 
#'   no.contact.assigned=NA
#' )
#' 
#' # reduced margins
#' par(mar=c(1,1,1,2))
#' # adjust default y-offset and depth scaling for following examples
#' plotSPC(sp1, y.offset=10, scaling.factor=0.5)
#' 
#' # get plotting parameters for profile widths and depth scaling factors
#' lsp <- get("last_spc_plot", envir = aqp.env)
#' 
#' # positions on x-axis, same for both depth and top "Bt" horizon
#' x.positions <- (1:length(sp1)) - lsp$width
#' 
#' # annotate contact with unicode right-arrow
#' # y-position is adjusted based on plot y-offset and scaling factor
#' y.positions <- lsp$y.offset + (sp1$depth * lsp$scaling.factor)
#' text(x.positions, y.positions, '\u2192', col='red', adj=1, cex=1.25, lwd=2)
#' 
#' # annotate top "Bt" depth with unicode right-arrow
#' # y-position is adjusted based on plot y-offset and scaling factor
#' y.positions <- lsp$y.offset + (sp1$top_Bt * lsp$scaling.factor)
#' text(x.positions, y.positions, '\u2192', col='blue', adj=1, cex=1.25, lwd=2)
#' 
#' 
#' \dontrun{
#'   # sample data
#'   data(gopheridge, package='soilDB')
#'   
#'   # run on a single profile
#'   estimateSoilDepth(gopheridge[1, ])
#'   
#'   # apply to an entire collection
#'   profileApply(gopheridge, estimateSoilDepth)
#' }


estimateSoilDepth <- function(f, name='hzname', p='Cr|R|Cd', selection = min, no.contact.depth=NULL, no.contact.assigned=NULL) {

  # sanity check: this function will only operate on an SPC
  if(! inherits(f, 'SoilProfileCollection')) {
    stop('`f` must be a SoilProfileCollection containing one profile', call. = FALSE)
  }
  
  # sanity check: this function works on SPC with length 1
  if(length(f) > 1) {
    stop('`f` can contain only one profile, see manual page for details')
  }
    
  
  # use SPC depth column names
  depthcols <- horizonDepths(f)
  top <- depthcols[1]
  bottom <- depthcols[2]
  
  ## TODO: guessing a horizon name isn't a good idea: this function requires that one be specified

  # if name is not in horizons, look if it is set in hzdesgncol
  hznames <- horizonNames(f)

  # if the user has not specified a column containing horizon designations
  if(! name %in% hznames) {
    name <- guessHzDesgnName(f)
    if(is.na(name)) {
      stop("soil depth estimation relies on a column containing horizon designations", call.=FALSE)
    }
  }

  # extract horizons
  h <- horizons(f)

  # extract possible contact
  contact.idx <- grep(p, h[[name]], ignore.case=TRUE)
  # everything else
  no.contact.idx <- grep(p, h[[name]], ignore.case=TRUE, invert=TRUE)
  
  ## TODO: this is really hard to follow, simplify / test
  
  # no contact defined, use deepest hz bottom depth
  if(length(contact.idx) < 1) {
    d <- max(h[[bottom]][no.contact.idx], na.rm=TRUE)

    # is there a user-specified depth at which we assume a standard depth?
    if(! is.null(no.contact.depth) & ! is.null(no.contact.assigned)) {
      if(d >= no.contact.depth & !is.null(no.contact.assigned)) {
        res <- no.contact.assigned
      } else {
        res <- d
      }
    } else {
      # otherwise use depth of deepest horizon
      res <- d
    }
      
  } else {
    # contact pattern matched
    # apply selection function
    res <- selection(h[[top]][contact.idx], na.rm=TRUE)
  }
    

  return(res)
}
