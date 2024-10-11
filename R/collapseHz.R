#' Collapse Horizons within Profiles Based on Pattern Matching
#'
#' Combines layers and aggregates data by grouping adjacent horizons which match `pattern` in
#' `hzdesgn` or, alternately, share a common value in `by` argument. Numeric properties are combined
#' using the weighted average, and other properties are derived from the dominant condition based on
#' thickness of layers and values in each group.
#'
#' @param x A _SoilProfileCollection_
#' @param pattern _character_. A regular expression pattern to match in `hzdesgn` column. Default:
#'   `NULL`.
#' @param by _character_. A column name specifying horizons that should be combined. Aggregation
#'   will be applied to adjacent groups of layers within profiles that have the same value in `by`.
#'   Used in lieu of `pattern` and `hzdesgn`. Default: `NULL`.
#' @param hzdesgn _character_. Any character column containing horizon-level identifiers. Default:
#'   `hzdesgnname(x, required = TRUE)`.
#' @param FUN _function_. A function that returns a _logical_ vector equal in length to the number
#'   of horizons in `x`. Used only when `pattern` is specified. See details.
#' @param ... Additional arguments passed to the matching function `FUN`.
#' @param AGGFUN _list_. A _named_ list containing custom aggregation functions. List element names
#'   should match the column name that they transform. The functions defined should take three
#'   arguments: `x` (a vector of horizon property values), `top` (a vector of top depths), and
#'   `bottom` (a vector of bottom depths). Default: `NULL` applies `weighted.mean()` to all numeric
#'   columns not listed in `ignore_numerics` and takes the dominant condition (value with greatest
#'   aggregate thickness sum) for all other columns. See details.
#' @param ignore_numerics _character_. Vector of column names that contain numeric values which
#'   should _not_ be aggregated using `weighted.mean()`. For example, soil color "value" and
#'   "chroma".
#' @param na.rm _logical_. If `TRUE` `NA` values are ignored when calculating min/max boundaries for
#'   each group and in weighted averages. If `FALSE` `NA` values are propagated to the result.
#'   Default: `FALSE`.
#'
#' @details
#'
#' If a custom matching function (`FUN`) is used, it should accept arbitrary additional arguments
#' via an ellipsis (`...`). It is not necessary to do anything with arguments, but the result should
#' match the number of horizons found in the input SoilProfileCollection `x`.
#'
#' Custom aggregation functions defined in the `AGGFUN` argument should either return a single
#' vector value for each group*column combination, or should return a _data.frame_ object with named
#' columns. If the input column name is used as a column name in the result _data.frame_, then the
#' values of that column name in the result _SoilProfileCollection_ will be replaced by the output
#' of the aggregation function. See examples.
#' 
#' @return A _SoilProfileCollection_
#' 
#' @author Andrew G. Brown
#' 
#' @seealso `hz_dissolve()`
#' 
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
#' i <- collapseHz(jacobs2000_gen, by = "genhz")
#' 
#' profile_id(i) <- paste0(profile_id(i), "_collapse")
#' 
#' plot(
#'   c(i, jacobs2000),
#'   color = "genhz",
#'   name = "name",
#'   name.style = "center-center",
#'   cex.names = 1
#' )
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
#' 
#' # custom aggregation function for matrix_color_munsell (returns data.frame)
#' m <- collapseHz(jacobs2000,
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
#'                       data.frame(matrix_color_munsell = paste0(x, collapse = ";"),
#'                                  n_matrix_color = length(x))
#'                     } else {
#'                       data.frame(matrix_color_munsell = x, 
#'                                  n_matrix_color = length(x))
#'                     }
#'                   }
#'                 )
#'               )
#' profile_id(m) <- paste0(profile_id(m), "_collapse_custom")
#' 
#' m$matrix_color_munsell.n_matrix_color
collapseHz <- function(x,
                       pattern = NULL,
                       by = NULL,
                       hzdesgn = hzdesgnname(x, required = TRUE),
                       FUN = function(x, pattern, hzdesgn, ...) grepl(pattern, x[[hzdesgn]], ignore.case = FALSE),
                       ...,
                       AGGFUN = NULL,
                       ignore_numerics = NULL,
                       na.rm = FALSE) {
  idn <- idname(x)
  hzd <- horizonDepths(x)
  
  # use exact match of existing genhz labels as default in lieu of pattern
  if (is.null(pattern) & missing(by)) {
    by <- GHL(x, required = TRUE)
  }
  
  if (length(pattern) == 0) {
    pattern <- NA
  }
  
  # if a named vector of patterns is given, use the names as new labels
  if (!is.null(names(pattern))) {
    labels <- names(pattern)
    pattern <- as.character(pattern)
  } else {
    # otherwise, the patterns and labels are the same
    pattern <- as.character(pattern)
    labels <- pattern
  }
  
  h <- data.table::data.table(horizons(x))
  
  # iterate over patterns
  for (p in seq(pattern)) {
    
    # calculate matches
    if (!is.null(by) && length(pattern) == 1 && is.na(pattern)) {
      labels <- h[[by]]
      r <- rle(paste0(h[[idn]], "-", as.character(labels)))
      l <- rep(TRUE, nrow(h))
    } else {
      l <- FUN(x, pattern = pattern[p], hzdesgn = hzdesgn, na.rm = na.rm, ...)
      r <- rle(l)
    }
    
    # only apply aggregation if there are adjacent horizons that match the target criteria
    if (any(r$lengths > 1)) {
      g <- unlist(lapply(seq_along(r$lengths), function(i) rep(i, r$lengths[i])))
      hidx <- unlist(lapply(seq_along(r$lengths), function(i) if (r$lengths[i] == 1) TRUE else rep(FALSE, r$lengths[i]))) & l
      gidx <- g %in% unique(g[l]) & !hidx
      naf <- names(AGGFUN)
      
      # iterate over sets of layers needing aggregation within each matching group
      if (sum(gidx) > 0){
        res <- h[gidx, c(list(hzdeptnew = suppressWarnings(min(.SD[[hzd[1]]], na.rm = na.rm)), 
                              hzdepbnew = suppressWarnings(max(.SD[[hzd[2]]], na.rm = na.rm))),
                         
                              # process numeric depth weighted averages w/ dominant condition otherwise                       
                              sapply(colnames(.SD)[!colnames(.SD) %in% c(hzd, naf)],
                                       function(n, top, bottom) {
                                        v <- .SD[[n]]
                                        if (length(v) > 1) {
                                          if (!n %in% ignore_numerics && is.numeric(v)) {
    
                                            # weighted average by thickness (numerics not in exclusion list)
                                            v <- weighted.mean(v, bottom - top, na.rm = na.rm)
    
                                          } else {
                                            # take thickest value
                                            # v[which.max(bottom - top)[1]]
    
                                            # take dominant condition (based on sum of thickness)
                                            cond <- aggregate(bottom - top, by = list(as.character(v)), sum, na.rm = na.rm)
                                            v <- cond[[1]][which.max(cond[[2]])[1]]
                                          }
                                        }
                                        out <- data.frame(v)
                                        colnames(out) <- n
                                        out
                                      },
                                      top = .SD[[hzd[1]]],
                                      bottom = .SD[[hzd[2]]]),
                         
                             # process custom aggregation functions (may return data.frames)
                             do.call('c', lapply(colnames(.SD)[colnames(.SD) %in% naf], 
                                                 function(n, top, bottom) {
                                                           out <- AGGFUN[[n]](.SD[[n]], top, bottom)
                                                           if (!is.data.frame(out)) {
                                                             out <- data.frame(out)
                                                             colnames(out) <- n
                                                           } else {
                                                             colnames(out) <- paste0(n, ".", colnames(out))
                                                           }
                                                           out
                                                                             },
                                                 top = .SD[[hzd[1]]], 
                                                 bottom = .SD[[hzd[2]]]))), 
                 by = g[gidx]]
        # remove grouping ID
        res$g <- NULL
      } else {
        res <- h[0, ]
      }
      
      # allow for replacing values as well as adding new values with data.frame AGGFUN
      test1.idx <- na.omit(match(colnames(res), paste0(colnames(h), ".", colnames(h)))) 
      test2.idx <- na.omit(match(paste0(colnames(h), ".", colnames(h)), colnames(res)))
      colnames(res)[test2.idx] <- colnames(h)[test1.idx]
      
      # determine matches that are only a single layer (no aggregation applied)      
      res2 <- h[hidx & l, ]
      res2$hzdeptnew <- res2[[hzd[1]]]
      res2$hzdepbnew <- res2[[hzd[2]]]
      res2[[hzd[1]]] <- NULL
      res2[[hzd[2]]] <- NULL
      
      # combine matches
      res3 <- data.table::rbindlist(list(res, res2), fill = TRUE)
      if (missing(by) && nrow(res3) > 0){
        res3[[hzdesgn]] <- labels[p]
      }
      
      # combine matches with horizons that did not match
      agg.idx <- which(g %in% unique(g[l]) | hidx)
      if (length(agg.idx) > 0) {
        h <- h[-agg.idx, ]
      }
      h <- data.table::rbindlist(list(h, res3), fill = TRUE)
      
      # replace depths
      hn <- !is.na(h$hzdeptnew) & !is.na(h$hzdepbnew)
      h[[hzd[1]]][hn] <- h$hzdeptnew[hn]
      h[[hzd[2]]][hn] <- h$hzdepbnew[hn]
      h$hzdeptnew <- NULL
      h$hzdepbnew <- NULL
      
      # sort horizons by id name and top depth
      h <- h[order(h[[idn]], h[[hzd[1]]]),]
      
    }
    
    # replace horizons in parent SPC
    replaceHorizons(x) <- h
  }
  x
}

