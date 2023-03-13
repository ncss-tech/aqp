# split,SoilProfileCollection method is redundant aside from the ability to enter a site var by name
#
# data("loafercreek", package = "soilDB")
#
# loafercreek <- subset(loafercreek, !is.na(pmorigin))
# loafercreek$pmorigin <- factor(loafercreek$pmorigin)
#
# # split.default(SPC) and the aqp method are identical due to overloading of `[`
# microbenchmark::microbenchmark( default = { split.default(loafercreek, site(loafercreek)$pmorigin) },
#                                 aqp =     { split(loafercreek, "pmorigin") })
#

#' Split a SoilProfileCollection object into a list of SoilProfileCollection objects.
#'
#' This function splits a SoilProfileCollection into a list of SoilProfileCollection objects using a site-level attribute to define groups or profile ID (idname(x)).
#'
#' @param x a SoilProfileCollection object
#' @param f a character vector naming a single site-level attribute that defines groups, a ‘factor’ in the sense that \code{as.factor(f)} defines the grouping, or a list of such factors in which case their interaction is used for the grouping.
#' @param drop logical indicating if levels that do not occur should be dropped (if f is a factor or a list). When `drop=FALSE` and `f` contains missing values an additional group "<missing>" is returned.
#' @param ...	Additional arguments are ignored
#'
#' @details As of aqp 1.25, omission of `f` argument is no longer possible, as the base R generic is overloaded by this SoilProfileCollection method. This used to result in an "identity" split, according to \code{idname(x)}, e.g. a list as long as \code{length(x)}, with a single-profile SoilProfileCollection per list element. Replicate this behavior using \code{f = idname(x)} or \code{f = profile_id(x)}
#'
#' @author D.E Beaudette
#'
#' @return A list of SoilProfileCollections or \code{NULL} for empty result.
#' @export
#'
#' @examples
#'
#' data(sp2)
#' depths(sp2) <- id ~ top + bottom
#'
#' # add a more interesting site-level attribute
#' site(sp2) <- ~ surface
#'
#' # using identity site-level attribute (profile ID)
#' p1 <- split(sp2, f = idname(sp2))
#' names(p1)
#' length(p1)
#'
#' # using vector equal in length to number of profiles (profile ID, again)
#' p2 <- split(sp2, f = profile_id(sp2))
#' names(p2)
#' length(p2)
#'
#' # which are both equivalent to setting `f` to NULL
#' p3 <- split(sp2, f = NULL)
#' names(p3)
#' length(p3)
#'
#' # split on surface (age) site-level var
#' p4 <- split(sp2, f = "surface")
#' names(p4)
#' length(p4) # 5 unique "surfaces", 5 SPCs in result list
#'
setMethod("split",
          signature(x = "SoilProfileCollection", f = "ANY"),
          function(x, f, drop = TRUE, ...) {

  # identity split, use idname
  if (is.null(f)) {

    # grouping factor, make sure to use original ordering
    fg <- site(x)[[idname(x)]]
    fg <- factor(fg, levels = fg)

  } else {
    # standard, site-level group split
    # using an existing site-level attribute
    if (is.character(f) && length(f) == 1) {
      if (!f %in% siteNames(x)) {
        stop(sprintf('%s must be site-level attribute', f), call. = FALSE)
      }
      
      # extract to local variable, so as not to modify original data
      fg <- x[[f]]
      
      # no NA allowed
      if (any(is.na(fg)) && !drop) {
        fg[is.na(fg)] <- "<missing>"
      }
   
      # splitting variable should be a factor
      if (!inherits(fg, 'factor')) {
        fg <- factor(fg)
        message(sprintf('converting %s to factor', f))
      }

    # using a vector coercible to factor (like base::split)
    } else if (length(f) == length(x) && !is.list(f)) {

      # preserve NA levels as missing
      if (any(is.na(f)) && !drop) {
        f[is.na(f)] <- "<missing>"
      }

      if (!inherits(f, 'factor')) {
        fg <- factor(f)
      } else {
        fg <- f
      }
    } else if (is.list(f)) {

      # preserve NA levels as missing
      f <- lapply(f, function(ff) {
        if (any(is.na(ff)) && !drop) {
          ff[is.na(ff)] <- "<missing>"
        }
        ff
      })
      
      # using a interaction of a list coercible to factor (like base::split)
      fg <- interaction(f, drop = drop, sep = ".", lex.order = FALSE)

    } else {
      stop(sprintf('invalid argument `f`'), call. = FALSE)
    }
  }

  if (drop) {
    fg <- droplevels(fg)
  }

  # iterate over levels
  lv <- levels(fg)

  # index and split
  res <- lapply(lv, function(i) {

    # simple indexing on site-level data only
    rr <- x[which(fg == i), ]

    return(rr)
  })
  
  # save names
  names(res) <- lv

  # result is a list
  return(res)
})


# #note: RE: identity split with f not specified
#        unfortunately this S4 method for SPC,missing does not work
# #      f is required in the generic (no default)
# #      neither base nor data.table provide a default for f
#
# setMethod(f = "split",
#           signature(x = "SoilProfileCollection", f = "missing"),
#           function(x, f = NULL, drop = TRUE, ...) {
#               aqp::split(x, f = NULL, drop, ...)
#           })
#
# # sadly this doesnt work either :( (S3 definition where f has default)
# # alas I tried...
#
# split.SoilProfileCollection <- function(x, f = NULL, drop = TRUE, ...) {
#   aqp::split(x = x, f = f, drop = drop, ...)
# }

## S4 magic
# already exists, but we are modifying it... good idea or bad idea?
# setGeneric("split", function(x, f=NULL, drop=TRUE, ...) standardGeneric("split"))
#
#  we should not over-write generics if they exist. we use same method signature
#  but dispatch to x = SoilProfileCollection so we dont mask.
#  note that this causes problems for the identity split with no default `f` agb 2020/09/28
