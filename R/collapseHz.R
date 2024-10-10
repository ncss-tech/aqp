#' Collapse Horizons within Profiles Based on Pattern Matching
#'
#' Combines and aggregates data for layers by grouping adjacent horizons that
#' match `pattern` in `hzdesgn`. Numeric properties are combined using the 
#' weighted average, and other properties are derived from the thickest horizon
#' in each group.
#'
#' @param x A _SoilProfileCollection_
#' @param pattern _character_. A regular expression pattern to match in `hzdesgn`
#'  column
#' @param hzdesgn _character_. Any character column containing horizon-level
#'  identifiers. Default is estimated using `guessHzDesgnName()`.
#' @param FUN _function_. A function that returns a _logical_ vector equal in
#'  length to the number of horizons in `x`. See details.
#' @param ... Additional arguments passed to the matching function `FUN`.
#' @param AGGFUN _list_. A named list containing custom aggregation functions. 
#'  List element names should match the column name that they transform. The 
#'  functions defined should take three arguments: `x` (a vector of horizon 
#'  property values), `top` (a vector of top depths), and `bottom` (a vector of 
#'  bottom depths). Default: `NULL` applies weighted.mean() to all numeric
#'  columns not listed in `ignore_numerics` and takes the thickest value for all
#'  other columns.
#' @param ignore_numerics _character_. Vector of column names that contain numeric 
#'  values which should _not_ be aggregated using `weighted.mean()`. For example,
#'  soil color "value" and "chroma".
#' @param na.rm _logical_. If `TRUE` `NA` values are ignored when calculating
#'  min/max boundaries for each group and in weighted averages. If `FALSE` `NA`
#'   values are propagated to the result. Default: `FALSE`
#' 
#' @details
#' 
#' If a custom function (`FUN`) is used, it should accept arbitrary additional
#' arguments via an ellipsis (`...`). It is not necessary to do anything with
#' arguments, but the result should match the number of horizons found in the
#' input SoilProfileCollection `x`.
#' 
#' @return A _SoilProfileCollection_
#' @export
#'
#' @examples
#' data(jacobs2000)
#' 
#' a <- collapseHz(jacobs2000, c(`A` = "^A", 
#'                               `E` = "E", 
#'                               `Bt` = "[ABC]+t", 
#'                               `C` = "^C", 
#'                               `foo` = "bar"))
#' b <- jacobs2000
#' profile_id(a) <- paste0(profile_id(a), "_collapse")
#' 
#' plot(c(a, b), color = "clay")
#' 
#' # custom aggregation function for matrix_color_munsell
#' 
#' a2 <- collapseHz(jacobs2000, c(`A` = "^A", 
#'                               `E` = "E", 
#'                               `Bt` = "[ABC]+t", 
#'                               `C` = "^C", 
#'                               `foo` = "bar"), 
#'                AGGFUN = list(matrix_color_munsell = function(x, top, bottom) {
#'                               thk <- bottom - top
#'                               if (length(x) > 1) {
#'                                xord <- order(thk, decreasing = TRUE)
#'                                paste0(paste0(x[xord], " (t=", thk[xord], ")"), collapse = ", ")
#'                               } else x
#'                              })
#'                )
#' profile_id(a2) <- paste0(profile_id(a), "_collapse_custom")
#' 
#' unique(a2$matrix_color_munsell)
#' 
collapseHz <- function(x,
                       pattern,
                       hzdesgn = hzdesgnname(x, required = TRUE),
                       FUN = function(x, pattern, hzdesgn, ...) grepl(pattern, x[[hzdesgn]], ignore.case = FALSE),
                       ...,
                       AGGFUN = NULL,
                       ignore_numerics = NULL,
                       na.rm = FALSE) {
  idn <- idname(x)
  hzd <- horizonDepths(x)
  if (!is.null(names(pattern))) {
    labels <- names(pattern)
    pattern <- as.character(pattern)
  } else {
    pattern <- as.character(pattern)
    labels <- pattern
  }
  for (p in seq(pattern)) {
    h <- data.table::data.table(horizons(x))
    l <- FUN(x, pattern = pattern[p], hzdesgn = hzdesgn, na.rm = na.rm, ...)
    if (any(l)) {
      r <- rle(l)
      g <- unlist(sapply(seq(r$lengths), function(i) rep(i, r$lengths[i])))
      gidx <- g %in% unique(g[l])
      res <- h[gidx, c(list(hzdeptnew = min(.SD[[hzd[1]]], na.rm = na.rm), 
                            hzdepbnew = max(.SD[[hzd[2]]], na.rm = na.rm)),
                            sapply(colnames(.SD)[!colnames(.SD) %in% hzd], 
                                   function(n, top, bottom) {
                                    v <- .SD[[n]]
                                    if (n %in% names(AGGFUN)) {
                                      
                                      # custom aggregation function (column name specific)
                                      AGGFUN[[n]](v, top, bottom)
                                      
                                    } else if (!n %in% ignore_numerics && is.numeric(x)) {
                                      
                                      # weighted average by thickness (numerics not in exclusion list)
                                      weighted.mean(v, bottom - top, na.rm = na.rm)
                                      
                                    } else {
                                      # take thickest value
                                      # v[which.max(bottom - top)[1]]
                                      
                                      # take dominant condition (based on sum of thickness)
                                      cond <- aggregate(bottom - top, by = list(v), sum, na.rm = na.rm)
                                      cond[[1]][which.max(cond[[2]])[1]]
                                    }
                                  }, 
                                  top = .SD[[hzd[1]]], 
                                  bottom = .SD[[hzd[2]]])), 
               by = g[gidx]]
      res$g <- NULL
      res[[hzdesgn]] <- labels[p]
      h <- h[-which(g %in% unique(g[l])),]
      h <- data.table::rbindlist(list(h, res), fill = TRUE)
      h <- h[order(h[[idn]], h[[hzd[1]]]),]
      hn <- !is.na(h$hzdeptnew) & !is.na(h$hzdepbnew)
      h[[hzd[1]]][hn] <- h$hzdeptnew[hn]
      h[[hzd[2]]][hn] <- h$hzdepbnew[hn]
      h$hzdeptnew <- NULL
      h$hzdepbnew <- NULL
      replaceHorizons(x) <- h
    }
  }
  x
}

