#' Collapse Horizons within Profiles Based on Pattern Matching
#'
#' Combines and aggregates data for layers by grouping adjacent horizons that
#' match `pattern` in `hzdesgn`. Numeric properties are combined using the
#' weighted average, and other properties are derived from the thickest horizon
#' in each group.
#'
#' @param x A _SoilProfileCollection_
#' @param pattern _character_. A regular expression pattern to match in
#' `hzdesgn` column. Default
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
#' @param ignore_numerics _character_. Vector of column names that contain
#'  numeric  values which should _not_ be aggregated using `weighted.mean()`.
#'  For example, soil color "value" and "chroma".
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
#' # calculate a new SPC with genhz column based on patterns
#' new_labels <- c("A", "E", "Bt", "Bh", "C")
#' patterns <- c("A", "E", "B.*t", "B.*h", "C")
#' jacobs2000_gen <- generalizeHz(jacobs2000, new = new_labels, pattern = patterns)
#' 
#' # use existing generalized horizon labels
#' i <- collapseHz(jacobs2000_gen, hzdesgn = "genhz")
#' 
#' profile_id(i) <- paste0(profile_id(i), "_collapse")
#' plot(c(i, jacobs2000), color = "genhz", name = "name", name.style = "center-center", cex.names = 1)
#'  
#' # custom pattern argument  
#' j <- collapseHz(jacobs2000,
#'                 c(
#'                   `A` = "^A",
#'                   `E` = "E",
#'                   `Bt` = "[ABC]+t",
#'                   `C` = "^C",
#'                   `foo` = "bar"
#'                 ))
#' profile_id(j) <- paste0(profile_id(j), "_collapse")
#' plot(c(j, jacobs2000), color = "clay")
#' 
#' # custom aggregation function for matrix_color_munsell
#' k <- collapseHz(jacobs2000,
#'                 pattern = c(
#'                   `A` = "^A",
#'                   `E` = "E",
#'                   `Bt` = "[ABC]+t",
#'                   `C` = "^C",
#'                   `foo` = "bar"
#'                 ),
#'                 AGGFUN = list(
#'                   matrix_color_munsell = function(x, top, bottom) {
#'                     thk <- bottom - top
#'                     if (length(x) > 1) {
#'                       xord <- order(thk, decreasing = TRUE)
#'                       paste0(paste0(x[xord], " (t=", thk[xord], ")"), collapse = ", ")
#'                     } else
#'                       x
#'                   }
#'                 )
#'               )
#' profile_id(k) <- paste0(profile_id(k), "_collapse_custom")
#' 
#' unique(k$matrix_color_munsell)
#
collapseHz <- function(x,
                       pattern = NULL,
                       hzdesgn = hzdesgnname(x, required = TRUE),
                       FUN = function(x, pattern, hzdesgn, ...) grepl(pattern, x[[hzdesgn]], ignore.case = FALSE),
                       ...,
                       AGGFUN = NULL,
                       ignore_numerics = NULL,
                       na.rm = FALSE) {
  idn <- idname(x)
  hzd <- horizonDepths(x)
  
  if (is.null(pattern)) {
    pattern <- unique(as.character(x[[GHL(x, required = TRUE)]]))
  } 
  
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
      hidx <- unlist(sapply(seq(r$lengths), function(i) if (r$lengths[i] == 1) TRUE else rep(FALSE, r$lengths[i]))) & l
      gidx <- g %in% unique(g[l]) & !hidx
      
      res <- h[gidx, c(list(hzdeptnew = suppressWarnings(min(.SD[[hzd[1]]], na.rm = na.rm)), 
                            hzdepbnew = suppressWarnings(max(.SD[[hzd[2]]], na.rm = na.rm))),
                            sapply(colnames(.SD)[!colnames(.SD) %in% hzd], 
                                   function(n, top, bottom) {
                                    v <- .SD[[n]]
                                    if (length(v) > 1) {
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
                                    } else {
                                      v
                                    }
                                  }, 
                                  top = .SD[[hzd[1]]], 
                                  bottom = .SD[[hzd[2]]])), 
               by = g[gidx]]
      
      res$g <- NULL
      
      res2 <- h[hidx & l, ]
      res2$hzdeptnew <- res2[[hzd[1]]]
      res2$hzdepbnew <- res2[[hzd[2]]]
      res2[[hzd[1]]] <- NULL
      res2[[hzd[2]]] <- NULL
      
      res3 <- rbind(res, res2)
      
      res3[[hzdesgn]] <- labels[p]
      
      h <- h[-which(g %in% unique(g[l]) | hidx),]
      h <- data.table::rbindlist(list(h, res3), fill = TRUE)
      
      hn <- !is.na(h$hzdeptnew) & !is.na(h$hzdepbnew)
      h[[hzd[1]]][hn] <- h$hzdeptnew[hn]
      h[[hzd[2]]][hn] <- h$hzdepbnew[hn]
      h$hzdeptnew <- NULL
      h$hzdepbnew <- NULL
      
      h <- h[order(h[[idn]], h[[hzd[1]]]),]
      
      replaceHorizons(x) <- h
    }
  }
  x
}

