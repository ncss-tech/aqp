#' Collapse Horizons within Profiles Based on Pattern Matching
#'
#' Combines and aggregates layers by grouping adjacent horizons that match `pattern` in `hzdesgn`. Numeric properties are combined using the weighted average, and other properties are derived from the thickest horizon in each group.
#'
#' @param x A _SoilProfileCollection_
#' @param pattern _character_. A regular expression pattern to match in `hzdesgn` column
#' @param hzdesgn _character_. Any character column containing horizon-level identifiers. Default is estimated using `guessHzDesgnName()`.
#' @param ignore.case _logical_. If `FALSE`, the pattern matching is case sensitive and if `TRUE`, case is ignored during matching. Default: `FALSE`
#' @param na.rm _logical_. If `TRUE` `NA` values are ignored when calculating min/max boundaries for each group and in weighted averages. If `FALSE` `NA` values are propagated to the result. Default: `FALSE`
#' 
#' @return A _SoilProfileCollection_
#' @export
#'
#' @examples
#' data(jacobs2000)
#' 
#' a <- collapseHz(jacobs2000, c(`A` = "^A", `E` = "E", `Bt` = "[ABC]+t", `C` = "^C", `foo` = "bar"))
#' b <- jacobs2000
#' profile_id(a) <- paste0(profile_id(a), "_collapse")
#' 
#' plot(c(a, b), color = "clay")
collapseHz <- function(x, pattern, hzdesgn = guessHzDesgnName(x, required = TRUE), ignore.case = FALSE, na.rm = FALSE) {
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
    l <- grepl(pattern[p], h[[hzdesgn]], ignore.case = ignore.case)
    if (any(l)) {
      r <- rle(l)
      g <- unlist(sapply(seq(r$lengths), function(i) rep(i, r$lengths[i])))
      res <- h[g %in% unique(g[l]), c(list(hzdeptnew = min(.SD[[hzd[1]]], na.rm = na.rm), 
                                           hzdepbnew = max(.SD[[hzd[2]]], na.rm = na.rm)),
                                           lapply(.SD, \(x, top, bottom) {
                                             if (is.numeric(x)) {
                                                weighted.mean(x, bottom - top, na.rm = na.rm)
                                             } else {
                                                x[which.max(bottom - top)[1]]
                                             }
                                           }, .SD[[hzd[1]]], .SD[[hzd[2]]])), 
               by = g[g %in% unique(g[l])]]
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

