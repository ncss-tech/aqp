
## TODO: relationship to .pctMissing (1-unit slice eval)

## missing data fraction computed on a thickness basis
## this is meant to be run on a single profile at a time
# x: single SPC
# v: variables to consider
# n: horizon designations
# p: inverted pattern matching non-soil horizons
# m: method, 'relative' or 'absolute'
.getMissingDataFraction <- function(x, v, n, p, m) {
  # get horizons
  h <- horizons(x)
  # get top/bottom
  dc <- horizonDepths(x)
  # data fraction based on thickness
  hz.thick <- h[[dc[2]]] - h[[dc[1]]]

  # get all "soil" horizons, for named variables
  soil.hz <- grep(p, h[[n]], ignore.case=TRUE, invert=TRUE)

  # extract named columns, but only those from @horizons
  v <- as.vector(na.omit(match(v, horizonNames(x))))
  d <- h[soil.hz, v]

  # mask out non-soil horizons
  hz.thick <- hz.thick[soil.hz]

  # index hz with full data population
  hz.with.data <- which(complete.cases(d))

  # compute fraction of data based on:
  # relative thickness
  # absolute thickness
  total <- switch(m,
                  'relative' = sum(hz.thick, na.rm = TRUE),
                  'absolute' = 1
  )

  res <- sum(hz.thick[hz.with.data], na.rm=TRUE) / total

  return(res)
}


## iterate over profiles
# x: SPC with >=1 profiles
# vars: variables to consider
# name: horizon designations
# p: inverted pattern matching non-soil horizons
# method: 'relative' or 'absolute' reporting
#' Evaluate Missing Data
#'
#' Evaluate missing data in a SoilProfileCollection object
#'
#' Data completeness is evaluated by profile, based on the thickness of
#' horizons with complete horizon-level attribute values (specified in
#' \code{vars}) divided by the total thickness. The default REGEX pattern,
#' \code{p}, should catch most non-soil horizons which are excluded from the
#' evaluation.
#'
#' @param x a \code{SoilProfileCollection} object
#' @param vars a chatacter vector naming horizon-level attributes in \code{x}
#' @param name the name of a horizon-level attribute where horizon designations
#' are stored
#' @param p REGEX pattern used to match non-soil horizons
#' @param method 'relative' (proportion of total) or 'absolute' depths
#' @return A vector values ranging from 0 to 1 (\code{method='relative'}) or 0
#' to maximum depth in specified depth units (\code{method='absolute'}),
#' representing the quantity of non-NA data (as specified in \code{vars}) for
#' each profile.
#' @author D.E. Beaudette
#' @keywords manip
#' @examples
#'
#' # example data
#' data(sp2)
#'
#' # init SPC object
#' depths(sp2) <- id ~ top + bottom
#'
#' # compute data completeness
#' sp2$data.complete <- evalMissingData(sp2, vars = c('r', 'g', 'b'), name = 'name')
#' sp2$data.complete.abs <- evalMissingData(sp2, vars = c('r', 'g', 'b'),
#'                                          name = 'name', method = 'absolute')
#'
#' # rank
#' new.order <- order(sp2$data.complete)
#'
#' # plot along data completeness ranking
#' plot(sp2, plot.order=new.order, name='name')
#'
#' # add relative completeness axis
#' # note re-ordering of axis labels
#' axis(side=1, at=1:length(sp2), labels = round(sp2$data.complete[new.order], 2),
#'      line=-1.5, cex.axis=0.75)
#'
#' # add absolute completeness (cm)
#' axis(side=1, at=1:length(sp2), labels = sp2$data.complete.abs[new.order],
#'      line=1, cex.axis=0.75)
#'
#'
evalMissingData <- function(x, vars, name='hzname', p='Cr|R|Cd', method='relative') {
  # sanity check
  if(! method %in% c('relative', 'absolute')) {
    stop("method should be one of 'relative' or 'absolute'", call. = FALSE)
  }

  hn <- horizonNames(x)
  sdv <- setdiff(vars, hn)

  # check for bad hzname
  if(! name %in% hn) {
    stop(sprintf('`%s` not a horizon level attribute', name), call. = FALSE)
  }

  # check for bad hz attr
  if(length(sdv > 1)) {
    stop(sprintf('`%s` not horizon level attribute(s)', paste(sdv, collapse = ',')), call. = FALSE)
  }

  # apply missing data fraction eval to each profile
  md <- profileApply(x, .getMissingDataFraction, v=vars, n=name, p=p, m=method)
  return(md)
}

