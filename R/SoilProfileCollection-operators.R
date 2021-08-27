####
#### single-bracket SoilProfileCollection methods
####

### NOTE: this DOES NOT re-order data, only subsets!
##
## matrix / DF style access: only to horizon data
##
## i = profile index
## j = horizon / slice index
##
##
#' Matrix/data.frame-like access to profiles and horizons in a SoilProfileCollection
#'
#' @aliases  [,SoilProfileCollection-method
#'
#' @description You can access the contents of a SoilProfileCollection by profile and horizon "index", \code{i} and \code{j}, respectively: \code{spc[i, j, ...]}. Subset operations are propagated to other slots (such as diagnostics or spatial) when they result in removal of sites from a collection.
#'
#'  - \code{i} refers to the profile position within the collection. By default the order is based on the C SORT order of the variable that you specified as your unique profile ID at time of object construction. Note that if your ID variable was numeric, then it has been sorted as a character.
#'
#'  - \code{j} refers to the horizon or "slice" index. This index is most useful when either a) working with \code{slice}'d SoilProfileCollection or b) working with single-profile collections. \code{j} returns the layer in the specified index positions for all profiles in a collection.
#'
#'  - `...` is an area to specify an expression that is evaluated in the subset. Currently supported
#'
#'    - `.LAST` (last horizon in each profile): return the last horizon from each profile. This uses `i` but ignores the regular `j` index.
#'    - `.FIRST` (first horizon in each profile): return the last horizon from each profile. This uses `i` but ignores the regular `j` index.
#'    - `.HZID` (horizon index not `SoilProfileCollection` result): return the horizon indices corresponding to `i`+`j`+`...` ("k") constraints
#'
#' @param x a SoilProfileCollection
#' @param i a numeric or logical value denoting profile indices to select in a subset
#' @param j a numeric or logical value denoting horizon indices to select in a subset
#' @param ... non-standard expressions to evaluate in a subset
#' 
#' @param drop not used
#' @rdname singlebracket
#  SPC extract: "[" '[' single bracket SPC object extract method
setMethod("[", signature(x = "SoilProfileCollection",
                         i = "ANY",
                         j = "ANY"),  function(x, i, j, ...) {

                           # capture k right away
                           ksubflag <- FALSE

                           # 2nd value is first user-supplied expression
                           kargs <- substitute(list(...))
                           ksub <- as.character(kargs)[2:length(kargs)]

                           # handle special keywords in "k" index
                           for(k in ksub) {
                             if (!is.na(k)) {
                               switch(k,
                                      ".FIRST" = {
                                        j <- 1
                                      },
                                      ".LAST" = {
                                        ksubflag <- TRUE
                                      },
                                      ".HZID" = {
                                        ksubflag <- TRUE
                                      })
                             }
                           }

                           # convert to integer
                           if (!missing(i)) {
                             if (any(is.na(i))) {
                               stop('NA not permitted in profile index', call. = FALSE)
                             }

                             # convert logical to integer
                             # (thanks Jose Padarian for the suggestion!)
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


                           # this block does processing for logical or numeric j
                           # not special symbols like .LAST
                           if (!missing(j) & !ksubflag) {

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
                           h <- .as.data.frame.aqp(h, aqp_df_class(x))
                           pidx <- h[[idname(x)]] %in% p.ids
                           h <- h[pidx,]

                           if (ksubflag && ".HZID" %in% ksub) {

                             j.idx.allowed <- seq_len(nrow(x@horizons))[pidx]

                           } else {
                             j.idx.allowed <- seq_len(nrow(h))

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

                           }

                           # subset horizons/slices based on j --> only when j is given
                           if (!missing(j) | ksubflag) {

                             # faster replacement of j subsetting of horizon data
                             # if (aqp_df_class(x) == "data.table") {

                             h <- data.table::as.data.table(h)

                             # local vars to make R CMD check happy
                             .N <- NULL
                             .I <- NULL
                             V1 <- NULL

                             idn <- idname(x)

                             # by list @horizons idname (essentially iterating over profiles)
                             bylist <- list(h[[idn]])
                             names(bylist) <- idn

                             # handle special symbols in j index
                             # currently supported:
                             #  - .LAST: last horizon index per profile
                             #  - .HZID: return horizon slot row index (short circuit)
                             #  - .FIRST: first horizon index per profile
                             # the above can be combined. .LAST takes precedent over .FIRST.

                             # determine j indices to KEEP

                             # default is an index to each horizon
                             j.idx <- j.idx.allowed

                             if (ksubflag && ".LAST" %in% ksub) {
                               # trigger special last horizon case
                               j.idx <- h[, .I[.N], by = bylist]$V1
                             } else {
                               if (!missing(j)) {
                                 # get row indices of horizon data corresponding to j within profiles
                                 j.idx <- h[, .I[1:.N %in% j], by = bylist]$V1
                               }
                             }

                             # short circuit for horizon-slot indices
                             if (ksubflag && ".HZID" %in% ksub) {
                               return(j.idx.allowed[j.idx])
                             }

                             # determine which site indices to keep
                             # in case all horizons are removed, remove sites too
                             if (length(j.idx) == 0) {
                               i.idx <- numeric(0)
                             } else {
                               # determine which profiles to  KEEP
                               i.idx <- which(profile_id(x) %in% unique(h[j.idx,][[idn]]))
                             }

                             # }
                             # } else {
                             #   # retain a base R way of doing things (plenty fast with SPCs up to ~100k or so)
                             #   j.res <- as.list(aggregate(
                             #     h[[hzidname(x)]],
                             #     by = list(h[[idname(x)]]),
                             #     FUN = function(hh) {
                             #       list(1:length(hh) %in% j)
                             #     },
                             #     drop = FALSE
                             #   )$x)
                             #
                             #   ##  https://github.com/ncss-tech/aqp/issues/89
                             #   # fix #89, where i with no matching j e.g. @site data returned
                             #   i.idx <- which(as.logical(lapply(j.res, function(jr) { any(jr) })))
                             #
                             #   j.idx <-  which(do.call('c', j.res))
                             # }

                             # find any index out of bounds and ignore them
                             # j.idx.bad <- which(abs(j.idx) > nrow(h))
                             # i.idx.bad <- which(abs(i.idx) > nrow(s))
                             #
                             # if (length(i.idx))
                             #   i.idx <- i.idx[-i.idx.bad]
                             #
                             # if (length(j.idx))
                             #   j.idx <- j.idx[-j.idx.bad]

                             # do horizon subset with j index
                             h <- h[j.idx, ]

                             # if profiles have been removed based on the j-index constraints
                             if (length(i.idx) > 0) {
                               # remove sites that have no matching j
                               i.idx <- unique(c(s.i, i.idx))
                               s <- s.all[i.idx, , drop = FALSE]
                               h.ids <- s[[idname(x)]]

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
                                 sp <- sp[i.idx,]
                               }
                             }
                           }

                           rownames(h) <- NULL

                           # rebuild SPC object from slots
                           res <- SoilProfileCollection(
                             idcol = idname(x),
                             hzidcol = hzidname(x),
                             depthcols = horizonDepths(x),
                             metadata = aqp::metadata(x),
                             horizons = .as.data.frame.aqp(h, aqp_df_class(x)),
                             site = .as.data.frame.aqp(s, aqp_df_class(x)),
                             sp = sp,
                             diagnostic = .as.data.frame.aqp(d, aqp_df_class(x)),
                             restrictions = .as.data.frame.aqp(r, aqp_df_class(x))
                           )
                           
                           # preserve metadata that may have been customized relative to defaults
                           #  in prototype or resulting from construction of SPC
                           res <- .transfer.metadata.aqp(x, res)
                           
                           # fill in any missing data.frame class or group var
                           o.df.class <- aqp::metadata(x)$aqp_df_class
                           if(length(o.df.class) == 0) {
                             o.df.class <- "data.frame"
                           }

                           o.group.by <- aqp::metadata(x)$aqp_group_by
                           if(length(o.group.by) == 0) {
                             o.group.by <- ""
                           }

                           metadata(res)$aqp_df_class <- o.df.class
                           metadata(res)$aqp_group_by <- o.group.by
                           
                           # there should be as many records in @site as there are profile IDs
                           pid.res <- profile_id(res)
                           site.res <- site(res)[[idname(res)]]

                           if (length(pid.res) != length(site.res)) {
                             message("Some profiles have been removed from the collection.")
                           }

                           # the order of profile_ids should be the same as in @site
                           if (!all(pid.res == site.res)) {
                             warning("profile ID order does not match order in @site",
                                     call. = FALSE)
                           }

                           return(res)
                         })

####
#### double bracket SoilProfileCollection methods
####

#' Get column of horizon or site data in a SoilProfileCollection
#'
#' @name [[
#' @description
#'
#' Get the data from a column accessed by name. Column names other than profile ID are not shared between site and horizons.
#' Bonus: \code{[[} gives access to all site and horizon level variables in tab complete for RStudio using the magrittr pipe operator!
#'
#' @param x a SoilProfileCollection
#' @param i an expression resolving to a single column name in site or horizon table
#' @param j (not used)
#' @aliases [[,SoilProfileCollection,ANY-method, [[,SoilProfileCollection,ANY,ANY-method
#' @docType methods
#' @rdname doublebracket
#' @examples
#'
#' data(sp2)
#' depths(sp2) <- id ~ top + bottom
#' site(sp2) <- ~ surface
#'
#' # get with [[
#' sp2[['surface']]
#'
#' # get using "unknown" expression:
#' #  "2nd + 3rd horizon column names"
#' for(i in horizonNames(sp2)[2:3])
#'  print(sp2[[i]])
#'
#' data(sp5)
#'
#' # some column names to work with
#' rgb.columns <- c("R25","G25","B25")
#'
#' res <- lapply(rgb.columns, function(x) {
#'
#'   # [[ allows you to access column names in a loop
#'   round(sp5[[x]] * 255)
#'
#' })
#'
#' # rename scaled results
#' names(res) <- paste0(rgb.columns,"_scl")
#'
#' # add horizon ID to results
#' result <- data.frame(hzID = hzID(sp5), do.call('cbind', res))
#' head(result)
#'
#' # join result back into horizons
#' horizons(sp5) <- result

# accessor for site and horizon names via double bracket
#  site names in horizon names results return from site (idname)
#
# prevents:
#   "Error in object[[i]] : this S4 class is not subsettable"
#   which is an error caused by RStudio? when doing tab completion
#   with %>% operator on a SPC

setMethod("[[", signature(x = "SoilProfileCollection",
                          i = "ANY",
                          j = "ANY"),
          function(x, i, j) {
            if (length(i) == 1) {
              # site names take precedence for those
              #  shared between @site and @horizons (idname)
              if (i %in% siteNames(x))
                return(x@site[[i]])

              if (i %in% horizonNames(x))
                return(x@horizons[[i]])
            }
          })

#' Add or change column of horizon or site data in a SoilProfileCollection
#' @name [[<-
#' @description
#'
#' Add or change the data from a column accessed by name. Column names other
#' than profile ID are not shared between site and horizons. The benefit of
#' using double bracket setter over \code{$} is that \code{name} can be
#' calculated, whereas with \code{$}, it must be known a priori and hard coded.
#'
#' When using the double bracket setter the length of input and output matching
#' either the number of sites or number of horizons is used to determine which
#' slot new columns are assigned to.
#'
#' @param x a SoilProfileCollection
#' @param i an expression resolving to a single column name in site or horizon
#'   table-
#' @param value New value to replace -- unit length or equal in length to number
#'   of sites or horizons in the collection.
#'
#' @aliases [[<-,SoilProfileCollection,ANY,ANY-method
#' @docType methods
#' @rdname doublebracket-set
setReplaceMethod("[[", signature(x = "SoilProfileCollection",
                                 i = "ANY",
                                 value = "ANY"),
                 function(x, i, value) {
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
                       s <- data.frame(x@site, stringsAsFactors = FALSE)
                       s[[i]] <- value
                       x@site <- .as.data.frame.aqp(s, aqp_df_class(x))
                     } else {
                       stop("replacement length does not match number of profiles!",
                            call. = FALSE)
                     }
                   } else if (i %in% hznames | lv == nx) {
                     if (lv == nx | is.null(value)) {
                       h <- data.frame(x@horizons, stringsAsFactors = FALSE)
                       h[[i]] <- value
                       x@horizons <- .as.data.frame.aqp(h, aqp_df_class(x))
                     } else {
                       stop("replacement length does not match number of horizons!",
                            call. = FALSE)
                     }
                   } else {
                     if(!is.null(value))
                       stop("new data must match either number of profiles or number of horizons",
                          call. = FALSE)
                   }

                   return(x)

                 })


#' Get data from column of horizon or site data in a SoilProfileCollection
#' @name $
#' @description Get the data from a column accessed by name \code{x$name}. Column names other than profile ID are not shared between site and horizons.
#'
#' @param x a SoilProfileCollection
#' @param name a single column name in site or horizon table
#' @docType methods
#' @rdname dollarsign
#'
#' @aliases $,SoilProfileCollection-method
#' @examples
#'
#' data(sp1)
#'
#' depths(sp1) <- id ~ top + bottom
#'
#' # get data from a column by name (prop)
#' sp1$prop
#'
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

#' Set data in column of horizon or site data in a SoilProfileCollection
#' @name $<-
#' @description Set the data in a column accessed by name \code{spc$name}. Column names other than profile ID are not shared between site and horizons.
#'
#' When using \code{$<-}, the length of input and output matching either the number of sites or number of horizons is used to determine which slot new columns are assigned to. Use \code{site(x)$name <- value} or  \code{horizons(x)$name <- value} to be explicit about which slot is being accessed.
#'
#' @param x a SoilProfileCollection
#' @param name a single column name in site or horizon table
#' @docType methods
#' @aliases $<-,SoilProfileCollection-method
#' @param value Replacement values: unit length or equal to number of horizons or sites.
#' @rdname dollarsign-set
setReplaceMethod("$", signature(x = "SoilProfileCollection"),
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
                   if (name %in% names(s)) {
                     s[[name]] <- value
                     x@site <- s
                     return(x)
                   }

                   # ambiguous: use length of replacement to determine: horizon / site
                   n.site <- nrow(s)
                   n.hz <- nrow(h)
                   l <- length(value)

                   if (l == n.hz) {
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

