# if (!isGeneric("validSpatialData"))
  setGeneric("validSpatialData", function(object, ...)
    standardGeneric("validSpatialData"))

#' Check for valid spatial reference of profiles
#'
#' @description Are coordinate column names defined in metadata and existing in the SoilProfileCollection?
#' @param object a SoilProfileCollection
#' @aliases validSpatialData
#' @docType methods
#' @rdname validSpatialData
#' @return logical `TRUE` if column names are defined and correspond to existing data
#' @export
setMethod("validSpatialData", signature(object = "SoilProfileCollection"),
          function(object) {
            # coordinate column names are defined in metadata
            crds <- metadata(object)$coordinates
            
            # and columns of that name exist in either site or horizon slots
            return(!is.null(crds) && all(crds %in% names(object)))
          })

##
## overloads
##

# return a concatenated vector of horizon + site names
# note that we strip out the ID column name from @site

#' Get names of columns in site and horizons table
#'
#' @description Get names of columns in site and horizons table of a SoilProfileCollection.
#' @param x a SoilProfileCollection
#' @aliases names
#' @docType methods
#' @rdname names
#' @export
setMethod("names", signature("SoilProfileCollection"),
          function(x) {
            sn <- siteNames(x)
            res <- c(horizons = horizonNames(x), site = sn[!sn %in% idname(x)])
            return(res)
          })

# overload min() to give us the min depth within a collection
#' Get the minimum bottom depth in a SoilProfileCollection
#' @description Get the shallowest depth of description out of all profiles in a SoilProfileCollection. Data missing one or more of: bottom depth, profile ID, or any optional attribute are omitted using \code{complete.cases}.
#' @param x a SoilProfileCollection
#' @param v optional: a vector of horizon attribute names to refine calculation
#' @param na.rm remove `NA`? default: `TRUE`
#' @aliases min
#' @docType methods
#' @rdname min
#' @export
setMethod(
  f = "min",
  signature(x = "SoilProfileCollection"),
  definition = function(x, v = NULL, na.rm = TRUE) {

    htb <- horizonDepths(x)
    target.names <- c(idname(x), hzidname(x), htb)
    
    # optionally use a horizon-level property refine calculation
    if (!missing(v) && !is.null(v) && v %in% horizonNames(x)) {
      target.names <- c(target.names,  v)
    }
    
    # handle empty spc
    bd <- x@horizons[[htb[2]]]
    if (length(bd) == 0 || all(is.na(bd))) {
      return(Inf)
    }
    
    # filter out missing data, accounting for optional `v`
    h <- x@horizons
    idx <- which(complete.cases(.data.frame.j(h, target.names, aqp_df_class(x))))
    x@horizons <- h[idx,]
    
    # return the shallowest (of the deepest depths in each profile)
    # returning Inf with no non-missing arguments to min
    .LAST <- NULL
    .HZID <- NULL
    return(suppressWarnings(min(horizons(x)[x[,, .LAST, .HZID],][[htb[2]]], na.rm = na.rm)))
  }
)

# overload max() to give us the max depth within a collection
#' Get the maximum bottom depth in a SoilProfileCollection
#' @description Get the deepest depth of description out of all profiles in a SoilProfileCollection. Data missing one or more of: bottom depth, profile ID, or any optional attribute are omitted using \code{complete.cases}.
#' @param x a SoilProfileCollection
#' @param v optional: horizon-level column name to refine calculation
#' @param na.rm remove `NA`? default: `TRUE`
#' @aliases max
#' @docType methods
#' @rdname max
#' @export
setMethod(
  f = "max",
  signature(x = "SoilProfileCollection"),
  definition = function(x, v = NULL, na.rm = TRUE) {
    htb <- horizonDepths(x)
    target.names <- c(idname(x), hzidname(x), htb)
    
    # optionally use a horizon-level property refine calculation
    if (!missing(v) && !is.null(v) && v %in% horizonNames(x)) {
      target.names <- c(target.names,  v)
    }
    
    # handle empty spc
    bd <- x@horizons[[htb[2]]]
    if (length(bd) == 0 || all(is.na(bd))) {
      return(-Inf)
    }
    
    # filter out missing data, accounting for optional `v`
    h <- x@horizons
    idx <- which(complete.cases(.data.frame.j(h, target.names, aqp_df_class(x))))
    x@horizons <- h[idx,]
    
    
    # return the deepest (of the deepest depths in each profile)
    # returning -Inf with no non-missing arguments to max
    .LAST <- NULL
    .HZID <- NULL
    return(suppressWarnings(max(horizons(x)[x[,, .LAST, .HZID],][[htb[2]]], na.rm = na.rm)))
  }
)

# overload length() to give us the number of profiles in the collection
#' Get the number of profiles in a SoilProfileCollection
#' @description Get the number of profiles in a SoilProfileCollection
#' @param x a SoilProfileCollection
#' @aliases length
#' @docType methods
#' @rdname length
#' @export
setMethod(
  f = "length",
  signature(x = "SoilProfileCollection"),
  definition = function(x) {
    # faster replacement for profile_id()
    #   which calls unique(horizons(x)[[idname(x)]]) -- expensive
    l <- length(x@site[[idname(x)]])#profile_id(x))
    return(l)
  }
)

# overload nrow() to give us the number of horizons in the collection
# do not need to define a generic at all if we use the base prototype
  
#' Get the number of horizons in a SoilProfileCollection
#' @aliases nrow
#' @description Get the number of horizons in a SoilProfileCollection
#' @param x a SoilProfileCollection
#' @docType methods
#' @rdname nrow
#' @export
setMethod(
  f = "nrow",
  signature(x = "SoilProfileCollection"),
  definition = function(x) {
    nrow(x@horizons)
  }
)
  

#' @title Uniqueness within a `SoilProfileCollection` via MD5 Hash
#' @description Unique profiles within a `SoilProfileCollection` using and MD5 hash of select horizon and / or site level attributes.
#' 
#' @param x a `SoilProfileCollection`
#' @param vars Variables to consider in uniqueness.
#' @param SPC logical return a `SoilProfileCollection` when `TRUE`, otherwise vector of profile indices
#' 
#' @return `SoilProfileCollection` when `SPC = TRUE`, otherwise a vector of integers
#' 
#' @aliases unique
#' @docType methods
#' @rdname unique
#' @export
#' @examples
#'
#'   # an example soil profile
#'   x <- data.frame(
#'     id = 'A',
#'     name = c('A', 'E', 'Bhs', 'Bt1', 'Bt2', 'BC', 'C'),
#'     top = c(0, 10, 20, 30, 40, 50, 100),
#'     bottom = c(10, 20, 30, 40, 50, 100, 125),
#'     z = c(8, 5, 3, 7, 10, 2, 12)
#'   )
#'   
#'   # init SPC
#'   depths(x) <- id ~ top + bottom
#'   
#'   # horizon depth variability for simulation
#'   horizons(x)$.sd <- 2
#'   
#'   # duplicate several times
#'   x.dupes <- duplicate(x, times = 5)
#'   
#'   # simulate some new profiles based on example
#'   x.sim <- perturb(x, n = 5, thickness.attr = '.sd')
#'   
#'   # graphical check
#'   plotSPC(x.dupes, name.style = 'center-center')
#'   plotSPC(x.sim, name.style = 'center-center')
#'   
#'   # inspect unique results
#'   plotSPC(unique(x.dupes, vars = c('top', 'bottom')), name.style = 'center-center')
#'   
#'   # uniqueness is a function of variable selection
#'   plotSPC(unique(x.sim, vars = c('top', 'bottom')), name.style = 'center-center')
#'   plotSPC(unique(x.sim, vars = c('name')), name.style = 'center-center')
#'   
#'
#'
setMethod(f = 'unique',
          signature(x = "SoilProfileCollection"),
          definition = function(x, vars, SPC = TRUE) {
  
            if(!requireNamespace("digest", quietly = TRUE))
              stop("package `digest` is required", call.=FALSE)
            
            # compute hash by profile, for selected variables
            md5 <- profileApply(x, function(i) {
              # unlist in order to drop row names
              digest::digest(unlist(as(i, 'data.frame')[, vars]))
            })
            
            # get unique hashes
            u.md5 <- unique(md5)
            
            # list profile idx by hash:
            profiles.by.hash <- sapply(u.md5, function(i) which(md5 == i), simplify = FALSE)
            
            # get an index of the first copy of each profile
            u.profiles <- sapply(profiles.by.hash, function(i) i[1])
            
            # down-grade to un-named vector of indices
            u.profiles <- as.vector(u.profiles)
            
            if(SPC) {
              # return the unique set of profiles as an SPC
              return(x[u.profiles, ])
            } else {
              # return an index of unique profiles
              return(u.profiles)
            }
          }
)
  
#' @title Subset a SoilProfileCollection with logical expressions
#' @description \code{subset()} is a function used for extracting profiles from a SoilProfileCollection based on logical criteria. It allows the user to specify an arbitrary number of logical vectors (equal in length to site or horizon), separated by commas. The function includes some support for non-standard evaluation.
#'
#' @param x A SoilProfileCollection

#' @param ... Comma-separated set of R expressions that evaluate as TRUE or FALSE. Length for individual expressions matches number of sites OR number of horizons, in \code{object}.
#'
#' @param greedy Use "greedy" matching for combination of site and horizon level matches? \code{greedy = TRUE} is the union, whereas \code{greedy = FALSE} (default) is the intersection (of site and horizon matches).
#'
#'
#' @aliases subset
#' @export
#'
#' @details To minimize likelihood of issues with non-standard evaluation context, especially when using `subset()` inside another function, all expressions used in `...` should be in terms of variables that are in the site or horizon data frame.
#'
#' @return A SoilProfileCollection.
#'
#' @author Andrew G. Brown.
#'
setMethod("subset", signature(x = "SoilProfileCollection"),
          function(x, ..., greedy = FALSE) {
            object <- x

            # capture expression(s) at function
              .dots <- substitute(list(...))
              .dots <- .dots[2:length(.dots)]


              # create composite object to facilitate eval
              .data <- compositeSPC(object)

              # loop through list of quosures and evaluate
              res <- vector('list', length(.dots))
              for (i in 1:length(.dots)) {

                # why does n=2 work?!
                res[[i]] <- eval(.dots[[i]], .data, parent.frame(n = 2))

                # print(ls(envir=globalenv()))
                # print(res[[i]])
                # print(.dots[[i]])
              }
              res.l <- lapply(res, length)

              # distinguish site and horizon level attributes
              # in the expression input
              sitematch <- res[res.l == length(object)]
              horizonmatch <- res[res.l == nrow(object)]

              # intersect the multi-prop site constraints
              if (length(sitematch) > 1) {
                sm <- rowSums(do.call('cbind', sitematch))
                sitematch <- (sm == length(sitematch))
              }

              # intersect the multi-prop horizon constraints
              if (length(horizonmatch) > 1) {
                hm <- rowSums(do.call('cbind', horizonmatch))
                horizonmatch <- (hm == length(horizonmatch))
              }

              # empty value to hold site level index
              idx <- numeric()

              # create site level index from site criteria
              if (length(sitematch) == 1 | !is.list(sitematch))
                idx <- which(unlist(sitematch))

              # create site level index from matching horizon criteria
              if (length(horizonmatch) == 1 |
                  !is.list(horizonmatch)) {
                peiid.from.hz <- unique(object@horizons[[idname(object)]][unlist(horizonmatch)])
                hz.idx <- match(peiid.from.hz, profile_id(object))

                # check that we wont be filtering erroneously
                #
                integrity <- spc_in_sync(object)
                if(!integrity$valid) {
                  print(integrity)
                  stop("Unable to filter! SPC integrity checks failed!")
                }

                if (length(idx) & !greedy) {
                  # intersection of site and horizon level matches
                  idx <- idx[idx %in% hz.idx]
                } else if (greedy) {
                  # union of site and horizon level matches
                  idx <- c(idx, hz.idx)
                } else {
                  # if we have only horizon-level, use just horizon level
                  idx <- hz.idx
                }

              }

              # return SPC, subsetted using site level index
              return(object[na.omit(idx),])
          })

setGeneric("subsetHz", function(x, ...)
  standardGeneric("subsetHz"))

#' Subset the horizons in a SoilProfileCollection using logical criteria
#' 
#' \code{subsetHz()} is a function used for extracting horizons from a SoilProfileCollection based on logical criteria.
#' 
#' @param x a SoilProfileCollection
#' @param ... Comma-separated set of R expressions that evaluate as `TRUE` or `FALSE` in context of horizon data frame. Length for individual expressions matches number of horizons, in \code{x}.
#' 
#' @details To minimize likelihood of issues with non-standard evaluation context, especially when using `subsetHz()` inside another function, all expressions used in `...` should be in terms of variables that are in the horizon data frame.
#' 
#' @return a SoilProfileCollection with a subset of horizons, possibly with some sites removed
#' @export
#' @aliases subsetHz
#' @examples
#' 
#' data(sp3)
#' 
#' depths(sp3) <- id ~ top + bottom
#' 
#' # show just horizons with 10YR hues
#' plot(subsetHz(sp3, hue == '10YR'))
#' 
setMethod("subsetHz", signature(x = "SoilProfileCollection"), function(x, ...) {
  # capture expression(s) at function
  .dots <- substitute(list(...))
  .dots <- .dots[2:length(.dots)]
  
  # create composite object to facilitate eval
  .data <- horizons(x)
  
  # loop through list of expressions and evaluate
  res <- vector('list', length(.dots))
  for (i in 1:length(.dots)) {
    res[[i]] <- eval(.dots[[i]], .data, parent.frame(n = 2))
  }
  
  subcrit <- Reduce('&', res)
  
  if (!is.logical(subcrit)) {
    badxpr <- paste0("'",paste0(.dots[sapply(.dots, function(x) !is.logical(x))],
                                collapse=",'"),"'")
    message(sprintf("%s is not logical; returning `x` unchanged", badxpr))
    return(x)
  }
  
  newhz <- .data[which(subcrit),]
  
  # subset SPC first to remove sites and other slots
  x <- x[which(profile_id(x) %in% newhz[[idname(x)]]),]
  
  # then replace horizons with horizon subset 
  #   (avoid profile IDs in site are missing from replacement horizons!)
  replaceHorizons(x) <- newhz
  x
})

# functions tailored for use with magrittr %>% operator / tidyr

#' @title Subset SPC with pattern-matching for text-based attributes
#' @name grepSPC
#' @aliases grepSPC,SoilProfileCollection-method
#' @description \code{grepSPC()} is a shorthand function for subsetting SoilProfileCollections. For example, by \code{filter(grepl(spc, ...))} or \code{filter(stringr::str_detect(spc, ...))}. It provides pattern matching for a single text-based site or horizon level attribute.
#' @param object A SoilProfileCollection
#' @param attr A character vector (column in object) for matching patterns against.
#' @param pattern REGEX pattern to match in \code{attr}
#' @param ... Additional arguments are passed to \code{grep()}
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname grepSPC
#' @export grepSPC

# if (!isGeneric("grepSPC"))
  setGeneric("grepSPC", function(object, attr, pattern, ...)
    standardGeneric("grepSPC"))

setMethod("grepSPC", signature(object = "SoilProfileCollection"),
          function(object, attr, pattern, ...) {

            # .Deprecated("subset")

              # capture expression(s) at function
              .dots <- substitute(attr)

              # create composite object to facilitate eval_tidy
              .data <- compositeSPC(object)

              # do eval of attr
              res <- .data_dots(.data, eval(.dots))

              # do the pattern matching
              idx <- grep(res, pattern = pattern, ...)

              # subset the SPC for result
              return(object[idx,])

          })

#' @title Subset SPC based on result of performing function on each profile
#' @name subApply
#' @aliases subApply,SoilProfileCollection-method
#' @description \code{subApply()} is a function used for subsetting SoilProfileCollections. It currently does NOT support for "tidy" lexical features in the \code{...} arguments passed to \code{profileApply()}. The expectation is that the function \code{.fun} takes a single-profile SoilProfileCollection and returns a logical value of length one. The use case would be for any logical comparisons that cannot be evaluated inline by \code{subSPC()} because they require more than simple logical operations.
#' @param object A SoilProfileCollection
#' @param .fun, A function that takes a single profile, returns _logical_ of length 1.
#' @param ... Additional arguments are passed to \code{.fun}
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname subApply
#' @export subApply

# if (!isGeneric("subApply"))
  setGeneric("subApply", function(object, .fun, ...)
    standardGeneric("subApply"))

setMethod("subApply", signature(object = "SoilProfileCollection"),
          function(object, .fun, ...) {

              # .Deprecated("profileApply")

              # apply .fun to elements of x
              res <- profileApply(object, FUN = .fun, ...)

              # return subset of x where .fun is true
              return(object[which(res), ])
          })

## subset method for SoilProfileCollection objects
## s: site-level subsetting criteria (properly quoted)
## h: horizon-level subsetting criteria (properly quoted)
## result: SoilProfileCollection with all profiles that match _either_ criteria- i.e. greedy matching
# if (!isGeneric("subsetProfiles"))
  setGeneric("subsetProfiles", function(object, s, h, ...)
    standardGeneric("subsetProfiles"))

#' DEPRECATED use subset
#'
#' This function is used to subset \code{SoilProfileCollection} objects using
#' either site-level or horizon-level attributes, or both.
#'
#' The \code{s} argument supplies a fully-quoted search criteria for matching
#' via site or horizon-level attributes. The \code{h} argument supplies a fully-quoted
#' search criteria for matching via horizon-level attributes. All horizons
#' associated with a single horizon-level match (i.e. out of several, only a
#' single horizon matches the search criteria) are returned. See examples for
#' usage.
#' @name subsetProfiles
#' @aliases subsetProfiles,SoilProfileCollection-method
#' @param object object
#' @param s fully-quoted search criteria for matching
#' via site-level attributes
#' @param h fully-quoted search criteria for matching
#' via horizon-level attributes
#' @param ... not used
#'
#' @return A \code{SoilProfileCollection} class object.
#' @export
#' @examples
#'
#' # more interesting sample data
#' data(sp2)
#' depths(sp2) <- id ~ top + bottom
#' site(sp2) <- ~ surface
#'
#' # subset by integer index, note that this does not re-order the profiles
#' plot(sp2[1:5, ])
#'
#' # generate an integer index via pattern-matching
#' idx <- grep('modesto', sp2$surface, ignore.case=TRUE)
#' plot(sp2[idx, ])
#'
#' # generate in index via profileApply:
#' # subset those profiles where: min(ph) < 5.6
#' idx <- which(profileApply(sp2, function(i) min(i$field_ph, na.rm=TRUE) < 5.6))
#' plot(sp2[idx, ])
#'
setMethod("subsetProfiles", signature(object = "SoilProfileCollection"),
          function(object, s, h, ...) {
            .Deprecated("subset")

            # sanity checks
            if (missing(s) & missing(h))
              stop('must provide either, site or horizon level subsetting criteria',
                   call. = FALSE)

            # extract parts
            s.d <- site(object)
            h.d <- horizons(object)
            id.col <- idname(object)
            object.ids <- profile_id(object)

            # subset using conventional data.frame methods
            if (!missing(s))
              s.d.sub.IDs <- subset(s.d, select = id.col, subset = eval(parse(text = s)))[, 1] # convert to vector
            else
              s.d.sub.IDs <- NA

            if (!missing(h))
              h.d.sub.IDs <- subset(h.d, select = id.col, subset = eval(parse(text = h)))[, 1] # convert to vector
            else
              h.d.sub.IDs <- NA

            # intersect IDs if s and h were used
            if (!missing(h) & !missing(s))
              matching.IDs <- intersect(s.d.sub.IDs, h.d.sub.IDs)

            # if only h, or only s were used, then
            else
              matching.IDs <- unique(na.omit(c(s.d.sub.IDs, h.d.sub.IDs)))

            # convert IDs into a numerical profile index
            # note: no matches results in idx == 0
            idx <- match(matching.IDs, object.ids)

            # subset SoilProfileCollection
            return(object[idx, ])
          })

