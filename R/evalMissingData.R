
## TODO: relationship to .pctMissing (1-unit slice eval)

## missing data fraction computed on a thickness basis
## this is meant to be run on a single profile at a time
# x: single SPC
# v: variables to consider
# n: horizon designations
# p: inverted pattern matching non-soil horizons
# m: method, 'relative', 'absolute', 'horizon' 
.getMissingDataFraction <- function(x, v, n, p, m) {
  # get horizons
  h <- horizons(x)
  
  # get top/bottom
  dc <- horizonDepths(x)
  
  # get all "soil" horizons, for named variables
  soil.hz <- grep(pattern = p, x = as.character(h[[n]]), invert = TRUE)
  
  # compute fraction of data based on:
  
  # fraction of each horizon
  # this requires a little more work
  if(m == 'horizon') {
    
    # select variables, use all horizons
    v <- as.vector(na.omit(match(v, horizonNames(x))))
    d <- h[, v]
    
    d <- as.matrix(d)
    res <- apply(d, 1, function(i) {
      length(which(complete.cases(i))) / ncol(d)
    })
    
    # set non-soil hz to NA
    res[-soil.hz] <- NA
    
  } else {
    # data fraction based on thickness
    hz.thick <- h[[dc[2]]] - h[[dc[1]]]
    
    # extract named columns, but only those from @horizons
    v <- as.vector(na.omit(match(v, horizonNames(x))))
    d <- h[soil.hz, v]
    
    # mask out non-soil horizons
    hz.thick <- hz.thick[soil.hz]
    
    # index hz with full data population
    hz.with.data <- which(complete.cases(d))
    
    # relative thickness
    # absolute thickness
    total <- switch(m,
                    'relative' = sum(hz.thick, na.rm = TRUE),
                    'absolute' = 1
    )
    
    res <- sum(hz.thick[hz.with.data], na.rm = TRUE) / total
  }
  

  return(res)
}


## iterate over profiles
# x: SPC with >=1 profiles
# vars: variables to consider
# name: horizon designations
# p: inverted pattern matching non-soil horizons
# method: 'relative' or 'absolute' reporting

#' @title Evaluate Missing Data within a SoilProfileCollection
#'
#' @description Evaluate missing data within a `SoilProfileCollection` object
#'
#' Data completeness is evaluated by profile or by horizon. Profile-level evaluation is based on the thickness of horizons (`method = absolute`) with complete horizon-level attributes (`vars`), optionally divided by the total thickness (`method = relative`). The REGEX pattern (`p`) is used to filter non-soil horizons from the calculation.
#'
#' @param x `SoilProfileCollection` object
#' 
#' @param vars character vector, naming horizon-level attributes in `x`
#' 
#' @param name character, the name of a horizon-level attribute where horizon designations are stored, defaults to `hzdesgnname(x)`
#' 
#' @param p character, REGEX pattern used to match non-soil horizons
#' 
#' @param method character, one of: 'relative' (proportion of total) depth, 'absolute' depth, or 'horizon' (fraction not-missing by horizon)
#' 
#' @return A vector values ranging from 0 to 1 (`method = 'relative'`) or 0 to maximum depth in specified depth units (`method = 'absolute'`) representing the quantity of non-missing data (as specified in `vars`) for each profile. When `method = 'horizon'` a non-missing data fraction is returned for each horizon.
#' 
#' @author D.E. Beaudette
#' @keywords manip
#' @export
#' @examples
#' 
#' # example data
#' data("jacobs2000")
#' 
#' # fully populated
#' plotSPC(jacobs2000, name.style = 'center-center', 
#'         cex.names = 0.8, color = 'time_saturated')
#' 
#' # missing some data
#' plotSPC(jacobs2000, name.style = 'center-center', 
#'         cex.names = 0.8, color = 'concentration_color')
#' 
#' # very nearly complete
#' plotSPC(jacobs2000, name.style = 'center-center', 
#'         cex.names = 0.8, color = 'matrix_color')
#' 
#' 
#' # variables to consider
#' v <- c('time_saturated', 'concentration_color', 'matrix_color')
#' 
#' # compute data completeness by profile
#' # ignore 2C horizons
#' jacobs2000$data.complete <- evalMissingData(
#'   jacobs2000, 
#'   vars = v, 
#'   method = 'relative',
#'   p = '2C'
#' )
#' 
#' jacobs2000$data.complete.abs <- evalMissingData(
#'   jacobs2000, 
#'   vars = v, 
#'   method = 'absolute',
#'   p = '2C'
#' )
#' 
#' # compute data completeness by horizon
#' # ignore 2C horizons
#' jacobs2000$hz.data.complete <- evalMissingData(
#'   jacobs2000, 
#'   vars = v, 
#'   method = 'horizon',
#'   p = '2C'
#' )
#' 
#' 
#' # "fraction complete" by horizon
#' plotSPC(
#'   jacobs2000, name.style = 'center-center', 
#'   cex.names = 0.8, color = 'hz.data.complete'
#' )
#' 
#' 
#' # rank on profile completeness
#' new.order <- order(jacobs2000$data.complete)
#' 
#' # plot along data completeness ranking
#' plotSPC(
#'   jacobs2000, name.style = 'center-center', 
#'   cex.names = 0.8, color = 'concentration_color', 
#'   plot.order = new.order
#' )
#' 
#' # add relative completeness axis
#' # note re-ordering of axis labels
#' axis(
#'   side = 1, at = 1:length(jacobs2000), 
#'   labels = round(jacobs2000$data.complete[new.order], 2),
#'   line = 0, cex.axis = 0.75
#' )
#' 
#' # add absolute completeness (cm)
#' axis(
#'   side = 1, at = 1:length(jacobs2000), 
#'   labels = jacobs2000$data.complete.abs[new.order],
#'   line = 2.5, cex.axis=0.75
#' )
#' 



evalMissingData <- function(x, vars, name = hzdesgnname(x), p = 'Cr|R|Cd', method = c('relative', 'absolute', 'horizon')) {
  # sanity check
  method <- match.arg(method)

  hn <- horizonNames(x)
  sdv <- setdiff(vars, hn)

  # check for missing horizon name
  if(name == '') {
    stop('horizon name not set, specify with `name` argument or set with `hzdesgnname()`', call. = FALSE)
  }
  
  # check for bad horizon name
  if(! name %in% hn) {
    stop(sprintf('`%s` not a horizon level attribute', name), call. = FALSE)
  }

  # check for bad hz attr
  if(length(sdv > 1)) {
    stop(sprintf('`%s` not horizon level attribute(s)', paste(sdv, collapse = ',')), call. = FALSE)
  }

  # apply missing data fraction eval to each profile
  md <- profileApply(x, .getMissingDataFraction, v = vars, n = name, p = p, m = method)
  return(md)
}

