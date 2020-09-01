# profileApply

if (!isGeneric("profileApply"))
 	setGeneric("profileApply", function(object, FUN,
 	                                    simplify = TRUE,
 	                                    frameify = FALSE,
 	                                    chunk.size = 100,
 	                                    column.names = NULL,
 	                                    ...) standardGeneric("profileApply"))
# made internal by AGB 2020/01/19
#
# analog to apply():
# operates by profile, returns vector of length(object) or nrow(object)
.profileApply <- function(object, FUN, simplify=TRUE, ...) {

		# get profile IDs
		pIDs <- profile_id(object)

		## TODO: does this same any time / memory?
		# pre-allocate list
		l <- vector(mode = 'list', length = length(pIDs))
		# must set list names based on expected assignment in for() loop
		names(l) <- pIDs

		# iterate over SPC, spliting into a list of SPC_i ... SPC_n
		for(i in seq_along(pIDs)) {
			pID <- pIDs[i]
			l[[pID]] <- do.call(FUN, list(object[i, ], ...))
		}

		# optionally simplify
		if(simplify)
			return(unlist(l))

		return(l)
}
#' Iterate over profiles in a SoilProfileCollection
#'
#' @name profileApply
#'
#' @description Iterate over all profiles in a SoilProfileCollection, calling \code{FUN} on a single-profile SoilProfileCollection for each step.
#'
#' @param object a SoilProfileCollection
#'
#' @param FUN a function to be applied to each profile within the collection
#'
#' @param simplify logical, should the result be simplified to a vector? default: TRUE; see examples
#'
#' @param frameify logical, should the result be collapsed into a data.frame? default: FALSE; overrides simplify argument; see examples
#'
#' @param chunk.size numeric, size of "chunks" for faster processing of large SoilProfileCollection objects; default: 100
#'
#' @param column.names character, optional character vector to replace frameify-derived column names; should match length of colnames() from FUN result; default: NULL
#'
#' @param ... additional arguments passsed to FUN
#'
#' @return When simplify is TRUE, a vector of length nrow(object) (horizon data) or of length length(object) (site data). When simplify is FALSE, a list is returned. When frameify is TRUE, a data.frame is returned. An attempt is made to identify idname and/or hzidname in the data.frame result, safely ensuring that IDs are preserved to facilitate merging profileApply result downstream.
#'
#' @aliases profileApply,SoilProfileCollection-method
#' @docType methods
#' @rdname profileApply
#' @examples
#'
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' # estimate soil depth using horizon designations
#' profileApply(sp1, estimateSoilDepth, name='name')
#'
#' # scale a single property 'prop' in horizon table
#' # scaled = (x - mean(x)) / sd(x)
#' sp1$d <- profileApply(sp1, FUN=function(x) round(scale(x$prop), 2))
#' plot(sp1, name='d')
#'
#' # compute depth-wise differencing by profile
#' # note that our function expects that the column 'prop' exists
#' f <- function(x) { c(x$prop[1], diff(x$prop)) }
#' sp1$d <- profileApply(sp1, FUN=f)
#' plot(sp1, name='d')
#'
#' # compute depth-wise cumulative sum by profile
#' # note the use of an anonymous function
#' sp1$d <- profileApply(sp1, FUN=function(x) cumsum(x$prop))
#' plot(sp1, name='d')
#'
#' # compute profile-means, and save to @site
#' # there must be some data in @site for this to work
#' site(sp1) <- ~ group
#' sp1$mean_prop <- profileApply(sp1, FUN=function(x) mean(x$prop, na.rm=TRUE))
#'
#' # re-plot using ranks defined by computed summaries (in @site)
#' plot(sp1, plot.order=rank(sp1$mean_prop))
#'
#' ## iterate over profiles, calculate on each horizon, merge into original SPC
#'
#' # example data
#' data(sp1)
#'
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' # calculate horizon thickness and proportional thickness
#' # returns a data.frame result with multiple attributes per horizon
#' thicknessFunction <- function(p) {
#'   hz <- horizons(p)
#'   depthnames <- horizonDepths(p)
#'   res <- data.frame(profile_id(p), hzID(p),
#'                     thk=(hz[[depthnames[[2]]]] - hz[[depthnames[1]]]))
#'   res$hz_prop <- res$thk / sum(res$thk)
#'   colnames(res) <- c(idname(p), hzidname(p), 'hz_thickness', 'hz_prop')
#'   return(res)
#' }
#'
#' # list output option with simplify=F, list names are profile_id(sp1)
#' list.output <- profileApply(sp1, thicknessFunction, simplify = FALSE)
#' head(list.output)
#'
#' # data.frame output option with frameify=TRUE
#' df.output <- profileApply(sp1, thicknessFunction, frameify = TRUE)
#' head(df.output)
#'
#' # since df.output contains idname(sp1) and hzidname(sp1),
#' # it can safely be merged by a left-join via horizons<- setter
#' horizons(sp1) <- df.output
#'
#' plot(density(sp1$hz_thickness, na.rm=TRUE), main="Density plot of Horizon Thickness")
#'
#' ## iterate over profiles, subsetting horizon data
#'
#' # example data
#' data(sp1)
#'
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' # make some fake site data related to a depth of some importance
#' sp1$dep <- profileApply(sp1, function(i) {round(rnorm(n=1, mean=mean(i$top)))})
#'
#' # custom function for subsetting horizon data, by profile
#' # keep horizons with lower boundary < site-level attribute 'dep'
#' fun <- function(i) {
#'   # extract horizons
#'   h <- horizons(i)
#'   # make an expression to subset horizons
#'   exp <- paste('bottom < ', i$dep, sep='')
#'   # subset horizons, and write-back into current SPC
#'   slot(i, 'horizons') <- subset(h, subset=eval(parse(text=exp)))
#'   # return modified SPC
#'   return(i)
#' }
#'
#' # list of modified SoilProfileCollection objects
#' l <- profileApply(sp1, fun, simplify=FALSE)
#'
#' # re-combine list of SoilProfileCollection objects into a single SoilProfileCollection
#' sp1.sub <- union(l)
#'
#' # graphically check
#' par(mfrow=c(2,1), mar=c(0,0,1,0))
#' plot(sp1)
#' points(1:length(sp1), sp1$dep, col='red', pch=7)
#' plot(sp1.sub)
#'
setMethod(f='profileApply', signature='SoilProfileCollection', function(object, FUN,
                        simplify = TRUE,
                        frameify = FALSE,
                        chunk.size = 100,
                        column.names = NULL,
                        ...) {
  if(simplify & frameify) {
    # simplify and frameify are both TRUE -- ignoring simplify argument
    simplify <- FALSE
  }

  # total number of profiles we have to iterate over
  n <- length(object)
  o.name <- idname(object)
  o.hname <- hzidname(object)

  # split the SPC of size n into chunk.size chunks
  chunk <- sort(1:n %% max(1, round(n / chunk.size))) + 1

  # we first iterate over list of chunks of size chunk.size, keeping lists smallish
  # by dividing by a tunable factor -- set as 100 by default
  # then we iterate through each chunk, calling FUN on each element (profile)
  # then, concatenate the result into a list (or vector if simplify == TRUE)
  res <- do.call('c', lapply(split(1:n, chunk), function(idx) {
    .profileApply(object[idx,], FUN, simplify, ...)
  }))

  # return profile IDs if it makes sense for result
  if(length(res) == length(object))
    names(res) <- profile_id(object)

  # combine a list (one element per profile) into data.frame result
  if(!simplify & frameify) {

    # make sure the first result is a data.frame (i.e. FUN returns a data.frame)
    if(is.data.frame(res[[1]])) {

      # make a big data.frame
      res <- as.data.frame(do.call('rbind', res), stringsAsFactors = FALSE)

      # get ids
      pid <- profile_id(object)
      hz.id <- hzID(object)

      if (!is.null(column.names))
        colnames(res) <- column.names

    } else {
      warning("first result is not class `data.frame` and frameify is TRUE. defaulting to list output.", call. = FALSE)
    }
  }

  if(simplify)
   return(unlist(res))

  return(res)
})
