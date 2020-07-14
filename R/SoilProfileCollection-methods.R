

#' Get names of columns in site table
#'
#' @name validSpatialData
#'
#' @description Are the contents of @sp valid: n x 2 matrix? If not, then contents of @sp in the SoilProfileCollection are an empty SpatialPoints object.
#' @param object a SoilProfileCollection
#' @aliases validSpatialData,SoilProfileCollection-method
#' @docType methods
#' @rdname validSpatialData
#'
if (!isGeneric("validSpatialData"))
  setGeneric("validSpatialData", function(object, ...)
    standardGeneric("validSpatialData"))

setMethod("validSpatialData", signature(object = "SoilProfileCollection"),
          function(object) {
            # n x 2 ---> valid / initialized coordinates
            # n x 1 ---> empty SP object
            res <- dim(coordinates(object))[[2]]
            if (res == 2)
              return(TRUE)
            else
              return(FALSE)
          })

##
## overloads
##

# return a concatenated vector of horizon + site names
# note that we strip out the ID column name from @site

#' Get names of columns in site and horizons table
#'
#' @name names
#'
#' @description Get names of columns in site and horizons table of a SoilProfileCollection.
#' @param x a SoilProfileCollection
#' @aliases names,SoilProfileCollection-method
#' @docType methods
#' @rdname names
#'
setMethod("names", signature("SoilProfileCollection"),
          function(x) {
            res <- c(horizons = horizonNames(x), site = siteNames(x)[-1])
            return(res)
          })

# overload min() to give us the min depth within a collection
#' Get the minimum bottom depth in a SoilProfileCollection
#'
#' @name min
#'
#' @description Get the shallowest depth of description out of all profiles in a SoilProfileCollection. Data missing one or more of: bottom depth, profile ID, or any optional attribute are omitted using \code{complete.cases}.
#' @param x a SoilProfileCollection
#' @param v optional: a vector of horizon attribute names to refine calculation
#' @aliases min,SoilProfileCollection-method
#' @docType methods
#' @rdname min
setMethod(
  f = 'min',
  signature(x = "SoilProfileCollection"),
  definition = function(x, v = NULL) {
    h <- x@horizons

    # get bottom depth column name
    hz_bottom_depths <- horizonDepths(x)[2]

    # handle empty spc
    if(length(x@horizons[[hz_bottom_depths]]) == 0)
      return(NA)

    # optionally use a horizon-level property refine calculation
    if (!missing(v)) {
      target.names <- c(hz_bottom_depths, idname(x), v)
    } else {
      target.names <- c(hz_bottom_depths, idname(x))
    }

    # filter out missing data
    h <- aqp:::.data.frame.j(h, target.names, aqp_df_class(x))
    h <- h[complete.cases(h),]

    # compute max depth within each profile
    if (aqp_df_class(x) == "data.table" & 
        requireNamespace("data.table") ) {
      
      # base R faster for big data with no existing key
      # but if the key is already set, then this is ~2x faster with 1M profiles (sorted numeric IDs)
      if (idname(x) %in% data.table::key(h)) {
        idn <- idname(x)
        # # with no key
        # user  system elapsed 
        # 7.26    5.52   16.83 
        # # with pre-set key
        # user  system elapsed 
        # 2.07    0.00    2.08  
        
        # cant invoke this for something like min/max probably -- might do better on linux
        # data.table::setkeyv(h, c(idn))
        
        dep <- h[[hz_bottom_depths]]
        d <- dep[h[, .I[hz_bottom_depths == max(hz_bottom_depths, na.rm = T)][1],
                     by = idn]$V1] 
        
        # return from here for data.table
        return(min(d, na.rm = TRUE))
      }
    }
    
    # tapply on a data.frame
    # user  system elapsed 
    # 4.39    0.00    4.39 
    system.time(d <- tapply(h[[hz_bottom_depths]], h[[idname(x)]], max, na.rm = TRUE))
    
    # return the shallowest (of the deepest depths in each profile)
    return(min(d, na.rm = TRUE))
  }
)

# overload max() to give us the max depth within a collection
#' Get the maximum bottom depth in a SoilProfileCollection
#'
#' @name max
#'
#' @description Get the deepest depth of description out of all profiles in a SoilProfileCollection. Data missing one or more of: bottom depth, profile ID, or any optional attribute are omitted using \code{complete.cases}.
#'
#' @param x a SoilProfileCollection
#' @param v optional: horizon-level column name to refine calculation
#' @aliases max,SoilProfileCollection-method
#' @docType methods
#' @rdname max
setMethod(
  f = 'max',
  signature(x = "SoilProfileCollection"),
  definition = function(x, v = NULL) {
    # get bottom depth column name
    h <- x@horizons
    hz_bottom_depths <- horizonDepths(x)[2]

    # handle empty spc
    if(length(h[[hz_bottom_depths]]) == 0)
      return(NA)

    # optionally use a horizon-level property refine calculation
    if (!missing(v)) {
      target.names <- c(hz_bottom_depths, idname(x), v)
    } else {
      target.names <- c(hz_bottom_depths, idname(x))
    }

    # filter out missing data
    h <- .data.frame.j(h, target.names, aqp_df_class(x))
    h <- h[complete.cases(h),]

    # compute max depth within each profile
    if (aqp_df_class(x) == "data.table" & 
        requireNamespace("data.table") ) {
      
      # base R faster for big data with no existing key
      # but if the key is already set, then this is ~2x faster with 1M profiles (sorted numeric IDs)
      if (idname(x) %in% data.table::key(h)) {
        idn <- idname(x)
        # # with no key
        # user  system elapsed 
        # 7.26    5.52   16.83 
        # # with pre-set key
        # user  system elapsed 
        # 2.07    0.00    2.08  
        
        # cant invoke this for something like min/max probably -- might do better on linux
        # data.table::setkeyv(h, c(idn))
        
        dep <- h[[hz_bottom_depths]]
        d <- dep[h[, .I[hz_bottom_depths == max(hz_bottom_depths, na.rm = T)][1],
                   by = idn]$V1] 
        
        # return from here for data.table
        return(max(d, na.rm = TRUE))
      }
      
    }
    
    # tapply on a data.frame
    # user  system elapsed 
    # 4.39    0.00    4.39 
    system.time(d <- tapply(h[[hz_bottom_depths]], h[[idname(x)]], max, na.rm = TRUE))
    
    # return the deepest depth (of the deepest depths in each profile)
    return(max(d, na.rm = TRUE))
  }
)

# overload length() to give us the number of profiles in the collection
#' Get the number of profiles in a SoilProfileCollection
#'
#' @name length
#'
#' @description Get the number of profiles in a SoilProfileCollection
#' @param x a SoilProfileCollection
#' @aliases length,SoilProfileCollection-method
#' @docType methods
#' @rdname length
setMethod(
  f = 'length',
  signature(x = "SoilProfileCollection"),
  definition = function(x) {
    # faster replacement for profile_id()
    #   which calls unique(horizons(x)[[idname(x)]]) -- expensive
    l <- length(x@site[[idname(x)]])#profile_id(x))
    return(l)
  }
)

# overload nrow() to give us the number of horizons in the collection
#' Get the number of horizons in a SoilProfileCollection
#'
#' @name nrow
#'
#' @description Get the number of horizons in a SoilProfileCollection
#' @param object a SoilProfileCollection
#' @aliases nrow,SoilProfileCollection-method
#' @docType methods
#' @rdname nrow
if (!isGeneric('nrow'))
  setGeneric('nrow', function(x)
    standardGeneric('nrow'))

setMethod(
  f = 'nrow',
  signature(x = "SoilProfileCollection"),
  definition = function(x) {
    nrow(x@horizons)
  }
)
#' Get the indexes of unique profiles in a SoilProfileCollection
#'
#' @name unique
#'
#' @description Calculate MD5 hash of each profile in a SoilProfileCollection for the specified variables.
#'
#' @param x a SoilProfileCollection
#' @param vars Variables to consider in uniqueness.
#' @aliases unique,SoilProfileCollection-method
#' @docType methods
#' @rdname unique
#' @examples
#'
#' data(sp5)
#'
#' # find indices where all specified vars are unique
#' #  these match the indices of all profiles in sp5
#' #  therefore, all profiles in sp5 are unique
#'
#' all(unique(sp5, vars=c("id","sand","silt","clay") == 1:length(sp5)))
#'
setMethod(f = 'unique',
          signature(x = "SoilProfileCollection"),
          definition = function(x, vars) {
  # compute hash by profile, for selected variables
  md5 <- profileApply(x, function(i) {
    # unlist in order to drop row names
    #
    if(!requireNamespace("digest", quietly = TRUE))
       stop("package `digest` is required", call.=FALSE)

    digest::digest(unlist(as(i, 'data.frame')[, vars]))
  })

  # get unique hashes
  u.md5 <- unique(md5)

  # list profile idx by hash:
  profiles.by.hash <-
    sapply(u.md5, function(i)
      which(md5 == i), simplify = FALSE)

  # get an index of the first copy of each profile
  u.profiles <- sapply(profiles.by.hash, function(i)
    i[1])

  # return an index of unique profiles
  # down-grade to un-named vector of indices
  return(as.vector(u.profiles))
})

#' @title Subset a SoilProfileCollection with logical expressions
#' @name filter
#' @description \code{filter()} is a function used for subsetting SoilProfileCollections. It allows the user to specify an arbitrary number of logical vectors (equal in length to site or horizon), separated by commas. The function includes some support for "tidy" lexical features -- specifically, access to site and horizon-level variables directly by name.
#' @param object A SoilProfileCollection
#' @param ... Comma-separated set of R expressions that evaluate as TRUE or FALSE. Length for individual expressions matche number of sites OR number of horizons, in \code{object}.
#' @param greedy Use "greedy" matching for combination of site and horizon level matches? \code{greedy=TRUE} is the union, whereas \code{greedy=FALSE} (default) is intersection
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname filter
#' @export filter
#' @aliases filter,SoilProfileCollection-method
#' @docType methods

if (!isGeneric("filter"))
  setGeneric("filter", function(object, ...)
    standardGeneric("filter"))

setMethod("filter", signature(object = "SoilProfileCollection"),
          function(object, ..., greedy = FALSE) {
            if(requireNamespace("rlang", quietly = TRUE)) {

              # capture expression(s) at function
              x <- rlang::enquos(...)

              # create composite object to facilitate eval_tidy
              data <- compositeSPC(object)

              # loop through list of quosures and evaluate
              res <- lapply(x, function(q) {
                r <- rlang::eval_tidy(q, data)
                return(r)
              })
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
            } else {
               stop("package 'rlang' is required for filter", .call=FALSE)
            }
          })

# functions tailored for use with magrittr %>% operator / tidyr
# formerly thisisnotapipe.R

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

if (!isGeneric("grepSPC"))
  setGeneric("grepSPC", function(object, attr, pattern, ...)
    standardGeneric("grepSPC"))

setMethod("grepSPC", signature(object = "SoilProfileCollection"),
          function(object, attr, pattern, ...) {
            if(requireNamespace("rlang", quietly = TRUE)) {
              # capture expression(s) at function
              x <- rlang::enquo(attr)

              # create composite object to facilitate eval_tidy
              data <- compositeSPC(object)

              # do tidy eval of attr
              res <- rlang::eval_tidy(x, data)

              # do the pattern matching
              idx <- grep(res, pattern = pattern, ...)

              # subset the SPC for result
              return(object[idx,])

            } else {
              stop("package 'rlang' is required for grepSPC", .call=FALSE)
            }
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

if (!isGeneric("subApply"))
  setGeneric("subApply", function(object, .fun, ...)
    standardGeneric("subApply"))

setMethod("subApply", signature(object = "SoilProfileCollection"),
          function(object, .fun, ...) {
            if(requireNamespace("rlang", quietly = TRUE)) {

              #TODO: figure out how to use eval helpers here

              ## capture expression(s) at function
              #.dots <- rlang::enquos(...)

              # apply .fun to elements of x
              res <- profileApply(object, FUN = .fun, ...)

              # return subset of x where .fun is true
              return(object[which(res), ])
            } else {
             stop("package 'rlang' is required for subApply", .call=FALSE)
            }
          })

## subset method for SoilProfileCollection objects
## s: site-level subsetting criteria (properly quoted)
## h: horizon-level subsetting criteria (properly quoted)
## result: SoilProfileCollection with all profiles that match _either_ criteria- i.e. greedy matching
if (!isGeneric("subsetProfiles"))
  setGeneric("subsetProfiles", function(object, s, h, ...)
    standardGeneric("subsetProfiles"))

setMethod("subsetProfiles", signature(object = "SoilProfileCollection"),
          function(object, s, h, ...) {
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
              s.d.sub.IDs <-
              subset(s.d, select = id.col, subset = eval(parse(text = s)))[, 1] # convert to vector
            else
              s.d.sub.IDs <- NA

            if (!missing(h))
              h.d.sub.IDs <-
              subset(h.d, select = id.col, subset = eval(parse(text = h)))[, 1] # convert to vector
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
            return(object[idx,])
          })

