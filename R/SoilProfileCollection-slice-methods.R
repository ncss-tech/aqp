#' Slicing of SoilProfileCollection Objects
#'
#' A method for "slicing" of SoilProfileCollection objects into constant depth intervals. Now deprecated, see `[dice()]`.
#'
#' @name slice-methods
#' 
#' @aliases get.slice slice.fast slice slice,SoilProfileCollection-method
#' 
#' @docType methods
#' 
#' @param object a SoilProfileCollection
#' 
#' @param fm A formula: either `integer.vector ~ var1 + var2 + var3` where
#' named variables are sliced according to `integer.vector` OR where all
#' variables are sliced according to `integer.vector`: `integer.vector ~ .`.
#' 
#' @param top.down logical, slices are defined from the top-down: `0:10` implies 0-11 depth units.
#' 
#' @param just.the.data Logical, return just the sliced data or a new `SoilProfileCollection` object.
#' 
#' @param strict Logical, should the horizonation be strictly checked for self-consistency?
#' 
#' @return Either a new `SoilProfileCollection` with data sliced according to `fm`, or a `data.frame`.
#' 
#' @section Details: By default, slices are defined from the top-down:
#' \code{0:10} implies 0-11 depth units.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso \code{\link{slab}}
#' 
#' @references D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for
#' quantitative pedology: A toolkit for soil scientists, Computers &
#' Geosciences, Volume 52, March 2013, Pages 258-268,
#' 10.1016/j.cageo.2012.10.020.
#' 
#' @keywords methods manip
#' @export
#' @rdname slice
#' @examples
#'
#' library(aqp)
#'
#' # simulate some data, IDs are 1:20
#' d <- lapply(1:20, random_profile)
#' d <- do.call('rbind', d)
#'
#' # init SoilProfileCollection object
#' depths(d) <- id ~ top + bottom
#' head(horizons(d))
#'
#' # generate single slice at 10 cm
#' # output is a SoilProfileCollection object
#' s <- dice(d, fm = 10 ~ name + p1 + p2 + p3)
#'
#' # generate single slice at 10 cm, output data.frame
#' s <- dice(d, 10 ~ name + p1 + p2 + p3, SPC = FALSE)
#'
#' # generate integer slices from 0 - 26 cm
#' # note that slices are specified by default as "top-down"
#' # result is a SoilProfileCollection
#' # e.g. the lower depth will always by top + 1
#' s <- dice(d, fm = 0:25 ~ name + p1 + p2 + p3)
#' par(mar=c(0,1,0,1))
#' plotSPC(s)
#'
#' # generate slices from 0 - 11 cm, for all variables
#' s <- dice(d, fm = 0:10 ~ .)
#' print(s)
#'
#' # compute percent missing, for each slice,
#' # if all vars are missing, then NA is returned
#' d$p1[1:10] <- NA
#' s <- dice(d, 10 ~ ., SPC = FALSE, pctMissing = TRUE)
#' head(s)
#'
#' \dontrun{
#' ##
#' ## check sliced data
#' ##
#'
#' # test that mean of 1 cm slices property is equal to the
#' # hz-thickness weighted mean value of that property
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' # get the first profile
#' sp1.sub <- sp1[which(profile_id(sp1) == 'P009'), ]
#'
#' # compute hz-thickness wt. mean
#' hz.wt.mean <- with(
#'   horizons(sp1.sub),
#'   sum((bottom - top) * prop) / sum(bottom - top)
#' )
#'
#' # hopefully the same value, calculated via slice()
#' s <- dice(sp1.sub, fm = 0:max(sp1.sub) ~ prop)
#' hz.slice.mean <- mean(s$prop, na.rm = TRUE)
#'
#' # they are the same
#' all.equal(hz.slice.mean, hz.wt.mean)
#' }
#'
slice.fast <- function(object, fm, top.down=TRUE, just.the.data=FALSE, strict=TRUE){

  ## Plan:
  #   1. message about future deprecation -> dice()
  # message('Note: aqp::slice() will be deprecated in aqp version 2.0\n--> Please consider using the more efficient aqp::dice()')
  
  #   2. deprecation -> dice() in aqp 2.0
  .Deprecated(new = 'dice', msg = 'slice() has been deprecated, please use the more efficient aqp::dice()')
  
  #   3. masking / removal of slice(), shortly there after
  
  
  ## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)

  # test for logical input
  if(! inherits(fm, "formula")) {
    stop('must provide a valid formula: ~ var1 + var2 + ...', call.=FALSE)
  }


  # extract components of the formula:
  formula <- stringr::str_c(deparse(fm, 500), collapse="")
  elements <- stringr::str_split(formula, stringr::fixed("~"))[[1]]
  formula <- lapply(stringr::str_split(elements, "[+*]"), str_trim)

  # TODO: this will have to be changed when we implement no LHS = all slices
  if (length(formula) > 2)
    stop("please provide a valid formula", call.=FALSE)

  # extract parsed formula components
  vars <- formula[[2]] # RHS, simple enough
  # LHS: could be either single integer or vector of slices
  z <- as.numeric(eval(parse(text=formula[[1]])))

  # get horizons + depth column names + ID column name
  h <- horizons(object)
  hd <- horizonDepths(object)
  top <- hd[1] ; bottom <- hd[2] # convenience vars
  id <- idname(object)
  id.order <- profile_id(object) # this is the original ordering of profiles

  # check for bogus left/right side problems with the formula
  if(any(z < 0) | any(is.na(z)))
    stop('z-slice must be >= 1', call.=FALSE)

  ## TODO: this will have to be updated for z-slices defined by data in @site
  if(! inherits(z,  c('numeric','integer'))) # bogus z-slice
		stop('z-slice must be either numeric or integer', call.=FALSE)

  # check for '.' --> all variables, minus ID/depths
  if(any(vars == '.')) {
  	nh <- names(h)
  	cols.to.remove.idx <- match(c(id, top, bottom), nh)
  	vars <- nh[-cols.to.remove.idx]
  }


  # check for column names that don't exist
  if(any(vars %in% names(h)) == FALSE) # bogus column names in right-hand side
		stop('column names in formula do not match any horizon data', call.=FALSE)


  ## extract all vars by slice_i
  # pre-allocate storage as list
  hd.slices <- vector(mode='list', length=length(z))
  # prepare an index for the list
  slice.idx <- seq_along(z)

  # convert h into an imutable data.frame for speed
  # h <- idata.frame(h)

  ## TODO: list-based processing faster
  # iterate over this index
  for(slice.i in slice.idx) {

    # extract all vars for current slice
    # TODO: this is wasteful as the entire pile of horizons is passed to get.slice in each iteration of the loop
    m.i.sub <- get.slice(h, id=id, top=top, bottom=bottom, vars=vars, z=z[slice.i], strict=strict)

    # join with original IDs in order to account for NA, or bad horizonation
    d <- data.frame(temp_id = id.order)
    names(d) <- id

    m.i <- merge(d, m.i.sub[!duplicated(m.i.sub[[id]]),], by = id, all.x = TRUE, sort = FALSE)

    # add depth range:
    # top-down, means that the slice starts from the user-defined depths (default)
    if(top.down) {
      m.i[[top]] <- z[slice.i]          # "top"
      m.i[[bottom]] <- z[slice.i] + 1   # "bottom"
    }
    # otherwise, the slice starts at the bottom (why would someone do this?)
    else {
      m.i[[top]] <- z[slice.i] - 1      # "top"
      m.i[[bottom]] <- z[slice.i]       # "bottom"
    }

    # save to the list
    hd.slices[[slice.i]] <- m.i
  }

  # convert list into DF
  hd.slices <- data.frame(data.table::rbindlist(hd.slices))

  # re-order by id, then top
  # keep only data we care about
  # note that we have a new column in there used to store pct not NA
  hd.slices <- hd.slices[order(match(hd.slices[[id]], id.order), hd.slices[[top]]), c(id, top, bottom, vars, '.pctMissing')]

  # if we just want the data:
  if(just.the.data)
    return(hd.slices)

  ## otherwise return an SPC, be sure to copy over the spatial data

  # init new SPC
  # if all horizon attr are requested then a warning will be issued
  # `hzID` is not a unique horizon ID, using `hzID_`
  suppressWarnings(depths(hd.slices) <- as.formula(paste(id, '~', top, '+', bottom)))

  # reset auto-generated horizon ID so that we know it is now the slice ID
  idx <- match(hzidname(hd.slices), horizonNames(hd.slices))
  horizonNames(hd.slices)[idx] <- 'sliceID'
  hzidname(hd.slices) <- 'sliceID'

  # # copy spatial data
  # hd.slices@sp <- object@sp

  # safely copy site data via JOIN
  site(hd.slices) <- site(object)

  # copy over any diagnostic features/restrictions
  hd.slices@diagnostic <- object@diagnostic
  hd.slices@restrictions <- object@restrictions

  # copy over metadata
  hd.slices <- .transfer.metadata.aqp(object, hd.slices)

  ## un-set horizon designation and texture class metadata if they aren't in the sliced data
  hn <- horizonNames(hd.slices)

  # hz designation
  # this will be TRUE if hzdesgnname(object) is unset ('')
  if(! hzdesgnname(object) %in% hn) {
    hzdesgnname(hd.slices) <- ''
  }

  # texture class
  # this will be TRUE if hztexclname(object) is unset ('')
  if(! hztexclname(object) %in% hn) {
    hztexclname(hd.slices) <- ''
  }

  # reset options:
  options(opt.original)

  # done
  return(hd.slices)
}

setGeneric("slice", function(object, fm, top.down = TRUE, just.the.data = FALSE, strict = TRUE) 
  standardGeneric("slice"))

#' @export
#' @rdname slice
setMethod(f = 'slice', signature(object = 'SoilProfileCollection'), slice.fast)


# this function is run on the horizon data, once for each depth slice
#' @param h Horizon data.frame
#' @param id Profile ID
#' @param top Top Depth Column Name
#' @param bottom Bottom Depth Column Name
#' @param vars Variables of Interest
#' @param z Slice Depth (index).
#' @param include Either `'top'` or `'bottom'`. Boundary to include in slice. Default: `'top'`
#' @param strict Check for logic errors? Default: `TRUE`
#'
#' @export
#' @rdname slice
get.slice <- function(h, id, top, bottom, vars, z, include='top', strict=TRUE) {
  
  ## TODO: this is likely very slow
  # 1. get indices to rows matchings current depth slice (z)
  # this is the default method
  if(include == 'top')
    idx <- which(z >= h[[top]] & z < h[[bottom]])
  # not sure why someone would use this approach, but include it anyways
  if(include == 'bottom')
    idx <- which(z > h[[top]] & z <= h[[bottom]])
  
  ## TODO: split -> list -> process -> combine
  # 2. extract data.frame along slice, and named vars + id
  h <- h[idx, c(id, vars)]
  
  # 2.5 compute fraction missing
  # if there is only 1 variable, don't try to compute this value
  # if all data are missing NA is returned
  h$.pctMissing <- apply(as.matrix(h[, vars]), 1, function(i, n=length(vars)) length(which(is.na(i))) / n)
  
  # 3. QA/QC
  # how many unique IDs?
  l.ids <- length(unique(h[[id]]))
  # how many rows in the result?
  n.res <- nrow(h)
  
  # more rows than IDs --> bad horizonation
  if(l.ids != n.res) {
    if(strict == TRUE) {
      # get offending IDs
      id.tab <- table(h[[id]])
      bad.ids <- paste(names(id.tab)[which(id.tab > 1)], collapse=', ')
      stop(paste('bad horizonation in IDs:', bad.ids), call.=FALSE)
    }
    
    # looser interp of the data... issue warning and return multuple rows/ID
    # join(..., match='first') will correct the problem
    else
      warning('Bad horizonation detected, first matching horizon selected. Use strict=TRUE to enforce QA/QC.')
  }
  
  # done: return subset of original data + pct not NA
  return(h)
}

