
## init

"SoilProfileCollection" <- function(idcol = 'id',
                                    depthcols = c('top', 'bottom'),
                                    metadata = data.frame(stringsAsFactors = FALSE),
                                    horizons = data.frame(stringsAsFactors = FALSE),
                                    site = data.frame(stringsAsFactors = FALSE),
                                    sp = new('SpatialPoints'),
                                    diagnostic = data.frame(stringsAsFactors = FALSE),
                                    restrictions = data.frame(stringsAsFactors = FALSE)) {
  
  # set metadata (default: data.frame, centimeters)
  hzclass <- class(horizons)[1]
  
  metadata <- data.frame(
    aqp_df_class = hzclass,
    depth_units = 'cm',
    stringsAsFactors = FALSE
  )
  
  # create object
  new(
    "SoilProfileCollection",
    idcol = idcol,
    depthcols = depthcols,
    metadata = metadata,
    horizons = .as.data.frame.aqp(horizons, hzclass),
    site = .as.data.frame.aqp(site, hzclass),
    sp = sp,
    diagnostic = .as.data.frame.aqp(diagnostic, hzclass),
    restrictions = .as.data.frame.aqp(restrictions, hzclass)
  )
}

## show
setMethod(f = 'show',
          signature(object = 'SoilProfileCollection'),
          function(object) {
            
            # stats for later
            n.profiles <- length(object)
            n.hz <- nrow(object)
            
            # count number of hz and site columns for reporting truncated listing
            n.hz.cols <- length(horizonNames(object))
            n.site.cols <- length(siteNames(object))
            
            # local copies
            s <- object@site
            h <- object@horizons
            
            # determine number of rows to show
            n.s <- nrow(s)
            n.h <- nrow(h)
            rows.show.s <- 1:pmin(6, nrow(s))
            rows.show.h <- 1:pmin(6, nrow(h))
            
            # handle zero row
            if (n.h == 0) rows.show.h <- 0
            if (n.s == 0) rows.show.s <- 0
            
            # subset rows
            h <- h[rows.show.s, , drop = FALSE]
            s <- s[rows.show.h, , drop = FALSE]
            
            # move IDs and depths, horizon designation if available
            hzd <- hzdesgnname(object)
            if (length(hzd) > 0) {
              
              # registered in a slot
              idx <- match(c(idname(object),
                             hzidname(object),
                             horizonDepths(object),
                             hzdesgnname(object)), names(h))
            } else {
              # undefined
              idx <-
                match(c(idname(object), hzidname(object), horizonDepths(object)), names(h))
            }
            
            # determine number of columns to show, and index to hz / site data
            # user sett-able
            show.cols <- getOption('.aqp.show.n.cols')
            
            h.n <- names(h)
            
            # handle empty case
            hz.show <- numeric(0)
            site.show <- numeric(0)
            
            if (length(h.n) > 0) {
              h <- h[, c(h.n[idx], h.n[-idx])]
              # # if defined, move horizon designation to the 3rd column
              # # missing horizon designation evaluates to character(0)
              # hzd <- hzdesgnname(object)
              # if (length(hzd) > 0) {
              #   idx <- match(hzd, names(h))
              #   h <- h[, c(names(h)[idx], names(h)[-idx])]
              # }
              
              # show first n
              hz.show <- seq(from = 1,
                             to = pmin(show.cols, n.hz.cols),
                             by = 1)
              site.show <- seq(from = 1,
                               to = pmin(show.cols, n.site.cols),
                               by = 1)
            }
            
            # column subseting
            if(length(hz.show) > 0) {
              h <- h[, hz.show, drop = FALSE]
              # generate text explaining truncated summary
              hz.txt <- sprintf(
                "\nHorizon Attributes (first %s of %s columns):\n------------------------------------------------\n",
                pmin(show.cols, n.hz.cols),
                n.hz.cols
              )
            } else {
              hz.txt <- "[EMPTY]\n"
            }
            
            if(length(site.show) > 0) {
              s <- s[, site.show, drop = FALSE]
              site.txt <- sprintf(
                "\nSite Attributes (first %s of %s columns):\n---------------------------------------------\n",
                pmin(show.cols, n.site.cols),
                n.site.cols
              )
            } else {
              site.txt <- "[EMPTY]\n"
            }
            
            # header
            header.txt <-
              sprintf(
                "SoilProfileCollection: %s profiles | %s horizons\nprofile ID: %s\nhorizon ID: %s\n",
                n.profiles,
                n.hz,
                idname(object),
                hzidname(object)
              )
            cat(header.txt)
            
            #   	if(n.profiles > 1)
            # 			cat("\nDepth range: ", min(object), "-", max(object), " ", depth_units(object), "\n", sep="")
            #
            
            # make note of additional hz attributes
            cat(hz.txt)
            print(h, row.names = FALSE)
            
            if(n.h > rows.show.h)
              cat('[... more horizons ...]\n')
            
            # make note of additional site attributes
            cat(site.txt)
            print(s, row.names = FALSE)
            if(n.s > rows.show.s)
              cat('[... more sites ...]\n')
            
            # presence of spatial data
            if (nrow(coordinates(object)) == n.profiles) {
              cat('\nSpatial Data:\n')
              show(object@sp@bbox)
              show(proj4string(object))
            }
            
          })

##
## accessors
##

## ID column name
if (!isGeneric("idname"))
  setGeneric("idname", function(object, ...)
    standardGeneric("idname"))

setMethod("idname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@idcol))

## horizon ID name
if (!isGeneric("hzidname"))
  setGeneric("hzidname", function(object, ...)
    standardGeneric("hzidname"))

setMethod("hzidname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@hzidcol))

## get horizon IDs
if (!isGeneric("hzID"))
  setGeneric("hzID", function(object, ...)
    standardGeneric("hzID"))

setMethod("hzID", signature(object = "SoilProfileCollection"),
          function(object) {
            return(object@horizons[[hzidname(object)]])
          })

## horizon designation name
if (!isGeneric("hzdesgnname"))
  setGeneric("hzdesgnname", function(object, ...)
    standardGeneric("hzdesgnname"))

## get column containing horizon designations (there is a setter of same name)
setMethod("hzdesgnname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@hzdesgncol))

## get horizon designations (no corresponding setter -- no need)
if (!isGeneric("hzDesgn"))
  setGeneric("hzDesgn", function(object, ...)
    standardGeneric("hzDesgn"))

setMethod("hzDesgn", signature(object = "SoilProfileCollection"),
          function(object) {
            
            h <- object@horizons
            hzd <- hzdesgnname(object)
            
            if (length(hzd)) {
              
              if (hzd %in% horizonNames(object)) {
                res <- h[[hzd]]
                return(res)
              }
              
            } else {
              
              stop("horizon designation name (",
                   hzd,
                   ") not in horizonNames().",
                   call. = FALSE)
            }
            
          })

## horizon texture class name
if (!isGeneric("hztexclname"))
  setGeneric("hztexclname", function(object, ...)
    standardGeneric("hztexclname"))

## get column containing horizon designations (there is a setter of same name)
setMethod("hztexclname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@hztexclcol))


## distinct profile IDs
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...)
    standardGeneric("profile_id"))

## relies on ordering of profile IDs in horizons matching site
##
##  TODO: eliminate implicit C SORT by unique -- what relies on this?
##
##        checks in functions that rebuild slots for integrity,
##        see commit 4f27a60 for fix for rare degenerate merge case
##        when joining in incomplete horizon data

setMethod("profile_id", signature(object = "SoilProfileCollection"),
          function(object)
            
            # ideally, we could rely on site(object)[[idname(object)]]
            
            unique(as.character(object@horizons[[idname(object)]])))


## horizon depth column names
if (!isGeneric("horizonDepths"))
  setGeneric("horizonDepths", function(object, ...)
    standardGeneric("horizonDepths"))

setMethod("horizonDepths", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@depthcols))


## spatial data: coordinates
setMethod("coordinates", signature(obj = "SoilProfileCollection"),
          function(obj) {
            return(coordinates(obj@sp))
          })


## site data
if (!isGeneric("site"))
  setGeneric("site", function(object, ...)
    standardGeneric("site"))

# retrieves the site data frame
setMethod("site", signature(object = "SoilProfileCollection"),
          function(object) {
            return(.as.data.frame.aqp(object@site, metadata(object)$aqp_df_class))
          })

## diagnostic horizons: stored as a data.frame
if (!isGeneric("diagnostic_hz"))
  setGeneric("diagnostic_hz", function(object, ...)
    standardGeneric("diagnostic_hz"))

setMethod(f = 'diagnostic_hz', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@diagnostic, aqp_df_class(object)))
          })

## restrictions: stored as data.frame
if (!isGeneric("restrictions"))
  setGeneric("restrictions", function(object, ...)
    standardGeneric("restrictions"))

setMethod(f = 'restrictions', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@restrictions, aqp_df_class(object)))
          })

## horizon data
# returns a data.frame with horizons data
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod(f = 'horizons', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@horizons, metadata(object)$aqp_df_class))
          })

## metadata
# returns a data.frame
if (!isGeneric("metadata"))
  setGeneric("metadata", function(object, ...)
    standardGeneric("metadata"))

setMethod(f = 'metadata', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(object@metadata)
          })

## aqp_df_class
##   data.frame class in use (from constructor)
## 
if (!isGeneric("aqp_df_class"))
  setGeneric("aqp_df_class", function(object, ...)
    standardGeneric("aqp_df_class"))

setMethod(f = 'aqp_df_class', signature(object = 'SoilProfileCollection'),
          function(object) {
            u <- as.character(metadata(object)[['aqp_df_class']])
            
            # all handles the logical(0) for undefined
            if (all(u == '')) {
              message("aqp_df_class metadata entry not found")
              warning("data.table and tbl_df in SoilProfileCollection data.frame slots are EXPERIMENTAL, defaulting to data.frame", call. = FALSE)
              return("data.frame")
            }
            
            return(u)
          })

## depth_units
# returns a data.frame
if (!isGeneric("depth_units"))
  setGeneric("depth_units", function(object, ...)
    standardGeneric("depth_units"))

setMethod(f = 'depth_units', signature(object = 'SoilProfileCollection'),
          function(object) {
            u <- as.character(metadata(object)[['depth_units']])
            # give a warning if not defined
            if (length(u) == 0 | u == '')
              message('depth_units have not been defined')
            return(u)
          })


## TODO: strip-out idname
## get site column names
if (!isGeneric("siteNames"))
  setGeneric("siteNames", function(object, ...)
    standardGeneric("siteNames"))

setMethod("siteNames", signature(object = "SoilProfileCollection"),
          function(object) {
            res <- names(object@site)
            return(res)
          })

## TODO: strip-out idname
## get horizon column names
if (!isGeneric("horizonNames"))
  setGeneric("horizonNames", function(object, ...)
    standardGeneric("horizonNames"))

setMethod("horizonNames", signature(object = "SoilProfileCollection"),
          function(object) {
            res <- names(object@horizons)
            return(res)
          })


## TODO: this seems stupid, NULL would be much simpler to reason over...
## are the contents of @sp valid: n x 2 matrix?
## if not, then contents of @sp is an empty SpatialPoints object
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
setMethod("names", signature(x = "SoilProfileCollection"),
          function(x) {
            res <- c(horizons = horizonNames(x), site = siteNames(x)[-1])
            return(res)
          })

# overload min() to give us the min depth within a collection
setMethod(
  f = 'min',
  signature(x = "SoilProfileCollection"),
  definition = function(x, v = NULL) {
    # get bottom depth column name
    hz_bottom_depths <- horizonDepths(x)[2]
    
    # optionally use a horizon-level property refine calculation
    if (!missing(v)) {
      # combine bottom depths with IDs and variable
      h <- x@horizons[, c(hz_bottom_depths, idname(x), v)]
    } else {
      # combine bottom depths with IDs
      h <- x@horizons[, c(hz_bottom_depths, idname(x))]
    }
    
    # filter out missing data
    h <- h[complete.cases(h),]
    # compute max by ID
    d <- tapply(h[, 1], h[, 2], max, na.rm = TRUE)
    
    # return the shallowest depth
    return(min(d, na.rm = TRUE))
  }
)

# overload max() to give us the max depth within a collection
setMethod(
  f = 'max',
  signature(x = "SoilProfileCollection"),
  definition = function(x, v = NULL) {
    # get bottom depth column name
    hz_bottom_depths <- horizonDepths(x)[2]
    
    # optionally use a horizon-level property refine calculation
    if (!missing(v)) {
      # combine bottom depths with IDs and variable
      h <- x@horizons[, c(hz_bottom_depths, idname(x), v)]
    }	else {
      # combine bottom depths with IDs
      h <- x@horizons[, c(hz_bottom_depths, idname(x))]
    }
    
    # filter out missing data
    h <- h[complete.cases(h),]
    # compute max by ID
    d <- tapply(h[, 1], h[, 2], max, na.rm = TRUE)
    
    # return the deepest depth
    return(max(d, na.rm = TRUE))
  }
)

# overload length() to give us the number of profiles in the collection
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


# overload unique() via digest eval of unique profiles
uniqueSPC <- function(x, vars) {
  # compute hash by profile, for selected variables
  md5 <- profileApply(x, function(i) {
    # unlist in order to drop row names
    digest(unlist(as(i, 'data.frame')[, vars]))
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
}

setMethod(f = 'unique',
          signature(x = "SoilProfileCollection"),
          definition = uniqueSPC)



## standard column access: search horizons, then site
setMethod("$", signature(x = "SoilProfileCollection"),
          function(x, name) {
            # get names from site and hz data
            s.names <- siteNames(x)
            h.names <- horizonNames(x)
            
            # ## note: warnings may be issued when using auto-complete feature in RStudio
            # # when site data are initialized from an external DF, it is possible that
            # # there will be duplicate column names
            # if((name %in% h.names) && (name %in% s.names)) {
            #   warning('column name is present in horizon and site data, extracting from horizon data only', call.=FALSE)
            # }
            
            # get column from horizon data
            if (name %in% h.names) {
              res <- x@horizons[[name]]
            } else {
              # otherwise check site data
              if (name %in% s.names) {
                res <- x@site[[name]]
              } else {
                # if still missing return NULL
                res <- NULL
              }
            }
            
            return(res)
          })

setReplaceMethod("$", "SoilProfileCollection",
                 function(x, name, value) {
                   #print(name)
                   
                   # extract hz and site data
                   h <- x@horizons
                   s <- x@site
                   
                   # working with horizon data
                   if (name %in% names(h)) {
                     h[[name]] <- value
                     x@horizons <- h
                     return(x)
                   }
                   
                   # working with site data  
                   if(name %in% names(s)) {
                     s[[name]] <- value
                     x@site <- s
                     return(x)
                   }
                   
                   # ambiguous: use length of replacement to determine: horizon / site
                   n.site <- nrow(s)
                   n.hz <- nrow(h)
                   l <- length(value)
                   
                   if(l == n.hz) {
                     h[[name]] <- value
                     x@horizons <- h
                     return(x)
                   }
                   
                   if(l == n.site) {
                     s[[name]] <- value
                     x@site <- s
                     return(x)
                   }
                   
                   # otherwise, there is a problem
                   stop('length of replacement must equal number of sites or number of horizons')
                 }
)
# setReplaceMethod("$", signature(x = "SoilProfileCollection"),
#                  function(x, name, value) {
#                    #
#                    # existing values: replace by name
#                    #
#                    # new values: use length of value to 
#                    #             determine site/horizon names
#                    #
#                    # cannot change site->horizon or vice versa
#                    # use NULL to remove a value from site or horizon
#                    x[[name]] <- value
#                    return(x)
#                   })

setReplaceMethod("[[", signature(x = "SoilProfileCollection",
                                 i = "ANY",
                                 j = "ANY"),
                 function(x, i, j, ...) {
                   lv <- length(value)
                   lx <- length(x)
                   nx <- nrow(x)
                   
                   hznames <- horizonNames(x)
                   stnames <- siteNames(x)
                   
                   # default to creating site var
                   #  as long as its not in horizon names
                   if ((!i %in% hznames) &
                       (i %in% stnames | lv == lx)) {
                     if (lv == lx | is.null(value)) {
                       # if(inherits(x@site, "data.table")) {
                       #   x@site[,eval(quote(list(i)))] <- value
                       # } else {
                       #   x@site[[i]] <- value
                       # }
                       # s <- as.data.frame.data.frame(x@site)
                       # s[[i]] <- value
                       # x@horizons <- aqp:::.as.data.frame.aqp(s, aqp_df_class(s))
                     } else {
                       stop("replacement length does not match number of profiles!",
                            call. = FALSE)
                     }
                   } else if (i %in% hznames | lv == nx) {
                     if (lv == nx | is.null(value)) {
                       if(inherits(x@horizons, "data.table")) {
                        x@horizons[, eval(substitute(i))] <- value
                       } else {
                        x@horizons[[i]] <- value
                       }
                       # h <- as.data.frame(x@horizons)
                       # h[[i]] <- value
                       # x@horizons <- aqp:::.as.data.frame.aqp(h, aqp_df_class(x))
                     } else {
                       stop("replacement length does not match number of horizons!",
                            call. = FALSE)
                     }
                   } else {
                     stop("new data must match either number of profiles or number of horizons",
                          call. = FALSE)
                   }
                   
                   return(x)
                   
                 })

#' @title Subset SPC with logical expressions
#' @name filter
#' @aliases filter,SoilProfileCollection-method
#' @description \code{filter()} is a function used for subsetting SoilProfileCollections. It allows the user to specify an arbitrary number of logical vectors (equal in length to site or horizon), separated by commas. The function includes some support for "tidy" lexical features -- specifically, access to site and horizon-level variables directly by name.
#' @param object A SoilProfileCollection
#' @param ... Comma-separated set of R expressions that evaluate as TRUE or FALSE. Length for individual expressions matche number of sites OR number of horizons, in \code{object}.
#' @param greedy Use "greedy" matching for combination of site and horizon level matches? \code{greedy=TRUE} is the union, whereas \code{greedy=FALSE} (default) is intersection
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname filter
#' @export filter
#'

if (!isGeneric("filter"))
  setGeneric("filter", function(object, ...)
    standardGeneric("filter"))

setMethod("filter", signature(object = "SoilProfileCollection"),
          function(object, ..., greedy = FALSE) {
            #if(requireNamespace("rlang")) {
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
              peiid.from.hz <- unique(horizons(object)[unlist(horizonmatch),
                                                       idname(object)])
              hz.idx <- match(peiid.from.hz, profile_id(object))
              
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
            object[na.omit(idx),]
            #  } else {
            #    stop("package 'rlang' is required", .call=FALSE)
            #  }
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
            #if(requireNamespace("rlang")) {
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
            
            #} else {
            #  stop("package 'rlang' is required", .call=FALSE)
            #}
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
            #if(requireNamespace("rlang")) {
            
            #TODO: figure out how to use eval helpers here
            
            ## capture expression(s) at function
            #.dots <- rlang::enquos(...)
            
            # apply .fun to elements of x
            res <- profileApply(object, FUN = .fun, ...)
            
            # return subset of x where .fun is true
            return(object[which(res), ])
            #} else {
            #  stop("package 'rlang' is required", .call=FALSE)
            #}
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

# accessor for site and horizon names via double bracket
#  site names in horizon names results return from site (idname)
#
# prevents:
#   "Error in object[[i]] : this S4 class is not subsettable"
#   which is an error caused by RStudio? when doing tab completion
#   with %>% operator on a SPC
#
# bonus:
#  gives access to all site and horizon level vars in tab complete!
setMethod("[[", signature(x = "SoilProfileCollection",
                          i = "character"),
          function(x, i) {
            if (length(i) == 1) {
              # site names take precedence for those
              #  shared between @site and @horizons
              if (i %in% siteNames(x))
                return(x@site[, i])
              
              if (i %in% horizonNames(x))
                return(x@horizons[, i])
            }
          })


### NOTE: this DOES NOT re-order data, only subsets!
##
## matrix / DF style access: only to horizon data
##
## i = profile index
## j = horizon / slice index
##
setMethod("[", signature(x = "SoilProfileCollection",
                         i = "ANY",
                         j = "ANY"),
          function(x, i, j) {
            # check for missing i and j
            if (missing(i) & missing(j)) {
              stop('must provide either a profile index or horizon/slice index, or both',
                   call. = FALSE)
            }
            
            # convert to integer
            if (!missing(i)) {
              if (any(is.na(i))) {
                stop('NA not permitted in profile index', call. = FALSE)
              }
              
              # convert logical to integer
              # (thanks Jos? Padarian for the suggestion!)
              if (is.logical(i)) {
                i <- (1:length(x))[i]
              }
              
              can.cast <- is.numeric(i)
              if (can.cast) {
                if (all(abs(i - round(i)) < .Machine$double.eps ^ 0.5)) {
                  i <- as.integer(i)
                } else {
                  stop("Numeric site index does not contain whole numbers.")
                }
              } else {
                stop("Failed to coerce site index to integer.")
              }
            } else {
              # if no index is provided, the user wants all profiles
              i <- 1:length(x)
            }
            
            if (!missing(j)) {
              # AGB added logical handling to horizon index
              if (is.logical(j)) {
                j <- (1:length(x))[j]
              }
              
              can.cast <- is.numeric(j)
              if (can.cast) {
                if (all(abs(j - round(j)) < .Machine$double.eps ^ 0.5)) {
                  j <- as.integer(j)
                } else {
                  stop("Numeric horizon/slice index does not contain whole numbers.")
                }
              } else {
                stop("Failed to coerce horizon/slice index to integer.")
              }
              
              if (any(is.na(j))) {
                stop('NA not permitted in horizon/slice index', call. = FALSE)
              }
            }
            
            # extract all site and horizon data
            h <- x@horizons
            s.all <- x@site
            
            # extract requested profile IDs
            p.ids <- s.all[[idname(x)]][unique(i)]
            
            # keep only the requested horizon data (filtered by profile ID)
            h <- h[h[[idname(x)]] %in% p.ids,]
            
            # keep only the requested site data, (filtered by profile ID)
            s.i <- which(s.all[[idname(x)]] %in% p.ids)
            
            # need to use drop=FALSE when @site contains only a single column
            s <- s.all[s.i, , drop = FALSE]
            
            # subset spatial data, but only if valid
            if (validSpatialData(x)) {
              sp <- x@sp[i]
            } else {
              # copy empty SpatialPoints object
              sp <- x@sp
            }
            
            # subset diagnostic data
            d <- diagnostic_hz(x)
            if (length(d) > 0) {
              d <- d[which(d[[idname(x)]] %in% p.ids),]
            }
            
            # subset restriction data
            r <- restrictions(x)
            if (length(r) > 0) {
              r <- r[which(r[[idname(x)]] %in% p.ids),]
            }
            
            # subset horizons/slices based on j --> only when j is given
            if (!missing(j)) {
              # faster replacement of j subsetting of horizon data
              j.res <- as.list(aggregate(
                h[[hzidname(x)]],
                by = list(h[[idname(x)]]),
                FUN = function(hh) {
                  list(1:length(hh) %in% j)
                },
                drop = FALSE
              )$x)
              j.idx <- which(do.call('c', j.res))
              h <- h[j.idx, ]
              
              ##  https://github.com/ncss-tech/aqp/issues/89
              # fix #89, where i with no matching j e.g. @site data returned
              i.missing <-
                which(as.logical(lapply(j.res, function(jr)
                  ! any(jr))))
              
              # if some profiles have been removed based on the j-index constraints
              if (length(i.missing) > 0) {
                # remove sites that have no matching j
                h.ids <- s[i.missing, idname(x)]
                s <- s[-i.missing, , drop = FALSE]
                
                # remove also: diagnostics
                d.idx <- which(d[[idname(x)]] %in% h.ids)
                if (length(d.idx) > 0) {
                  d <- d[-d.idx, , drop = FALSE]
                }
                
                # restrictions
                r.idx <- which(r[[idname(x)]] %in% h.ids)
                if (length(r.idx) > 0) {
                  r <- r[-r.idx, , drop = FALSE]
                }
                
                # spatial
                if (validSpatialData(x)) {
                  sp <- sp[-i.missing,]
                }
              }
            }
            
            rownames(h) <- NULL
            
            # rebuild SPC object from slots
            res <- SoilProfileCollection(
              idcol = idname(x),
              depthcols = horizonDepths(x),
              metadata = aqp::metadata(x),
              horizons = h,
              site = s,
              sp = sp,
              diagnostic = d,
              restrictions = r
            )
            
            # preserve one off slots that may have been customized relative to defaults
            #  in prototype or resulting from construction of SPC
            suppressMessages(hzidname(res) <- hzidname(x))
            suppressMessages(hzdesgnname(res) <- hzdesgnname(x))
            suppressMessages(hztexclname(res) <- hztexclname(x))
            
            # there should be as many records in @site as there are profile IDs
            pid.res <- profile_id(res)
            site.res <- site(res)[[idname(res)]]
            
            if (length(pid.res) != length(site.res)) {
              warning("Some profiles have been removed from the collection.", call. = FALSE)
            }
            
            # the order of profile_ids should be the same as in @site
            if (!all(pid.res == site.res)) {
              warning("profile ID order does not match order in @site", call. = FALSE)
            }
            
            return(res)
          })