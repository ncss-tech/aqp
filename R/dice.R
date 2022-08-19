
## https://github.com/ncss-tech/aqp/issues/115

## TODO: 
##   * add S4 interface
##   * suggest / offer repairMissingHzDepths() before running
##   * DT full outer join ideas
##     https://stackoverflow.com/questions/15170741/how-does-one-do-a-full-join-using-data-table


#' @title Efficient Slicing of `SoilProfileCollection` Objects
#' 
#' @description Cut ("dice") soil horizons into 1-unit thick slices. This function will eventually replace `aqp::slice()`.
#'
#'
#' @param x a `SoilProfileCollection` object
#' 
#' @param fm optional `formula` describing top depths and horizon level attributes to include: `integer.vector ~ var1 + var2 + var3` or `integer.vector ~ .` to include all horizon level attributes. When `NULL` profiles are "diced" to depth and results will include all horizon level attributes.
#' 
#' @param SPC return the diced `SoilPrfolileCollection`, if `FALSE` a `data.frame` of horizon-level attributes
#' 
#' @param pctMissing compute "percent missing data" by slice (when `TRUE` expect 6-8x longer run time)
#' 
#' @param fill logical, fill with empty placeholder horizons in gaps within profiles, and/or, above/below interval specified in `fm`. Automatically set to `TRUE` when `fm` is specified. Backwards compatibility with `slice` is maintained by setting `fill = TRUE` with or without `fm`.
#' 
#' @param strict perform horizon depth logic checking / flagging / removal
#' 
#' @param byhz Evaluate horizon depth logic at the horizon level (`TRUE`) or profile level (`FALSE`). Invalid depth logic invokes `HzDepthLogicSubset` which removes offending profiles or horizon records.
#' 
#' @param verbose Print information about object size/memory usage. Default: `FALSE`
#' 
#' @details For large and potentially messy collections that include missing horizon bottom depths, or 0-thickness horions, consider using `repairMissingHzDepths()` before `dice()`.
#' 
#'
#' @return a `SoilProfileCollection` object, or `data.frame` when `SPC = FALSE`
#' 
#' @author D.E. Beaudette, A.G. Brown 
#' 
#' @export
#' 
dice <-  function(x,
                  fm = NULL,
                  SPC = TRUE,
                  pctMissing = FALSE,
                  fill = FALSE,
                  strict = TRUE,
                  byhz = FALSE,
                  verbose = FALSE) {
    
  # sacrifice to R CMD check spirits
  .pctMissing <- NULL
  
  # find / flag / remove invalid profiles or horizons
  # this will generate an error if there are no valid profiles remaining
  if  (strict) {
    x <- HzDepthLogicSubset(x, byhz = TRUE)
    
    ## TODO: this could invoke 2x calls to fillHzGaps
    # removed horizons will trigger an automatic gap-filling
    if (!is.null(metadata(x)$removed.horizons)) {
      message('filling gaps left by HzDepthLogicSubset')
      x <- fillHzGaps(x, flag = TRUE, to_top = NULL, to_bottom = NULL)
    }
  }
  
  # keep track of original object size
  o.size <- object.size(x)
  
  ## extract pieces
  idn <- idname(x)
  hzidn <- hzidname(x)
  htb <- horizonDepths(x)
  hznames <- horizonNames(x)
  
  # IDs + top/bottom
  ids.top.bottom.idx <- match(c(idn, hzidn, htb), hznames)
  
  # interpret optional formula
  if (!is.null(fm)) {
    # something supplied
    # reasonable?
    if (!inherits(fm, "formula")) {
      stop('must provide a valid formula', call. = FALSE)
    }
    
    # extract components of the formula:
    fm <- str_c(deparse(fm, 500), collapse = "")
    elements <- str_split(fm, fixed("~"))[[1]]
    fm <- lapply(str_split(elements, "[+*]"), str_trim)

    # test for a multi-part formula A ~ B ~ C ?
    if (length(fm) > 2) {
      stop("please provide a valid formula", call. = FALSE)
    }
      
    # LHS ~ RHS
    # LHS: "", single integer, vector of integers slices
    z <- as.numeric(eval(parse(text = fm[[1]])))
    # RHS: variable names
    vars <- fm[[2]] 
    
    # check for bogus left/right side problems with the formula
    if (any(z < 0) | any(is.na(z))) {
      stop('z-slice must be >= 0', call. = FALSE)
    }
      
    # check for integer / numeric slices
    if (!inherits(z,  c('numeric','integer'))) {
      stop('z-slice must be either numeric or integer', call. = FALSE)
    }
      
    # if z-index is missing set to NULL for later on
    if (length(z) == 0) {
      z <- NULL
    } else {
      # z index is specified
      # must fill from min(z) --- [gaps] --- max(z)
      x <- fillHzGaps(x, flag = TRUE, to_top = min(z), to_bottom = max(z))
    }
    
    # check for '.' --> all variables, minus ID/depths
    if (any(vars == '.')) {
      # all variables except profile ID, horizon ID, top, bottom
      vars <- hznames[-ids.top.bottom.idx]
    }

    # check for column names that don't exist
    if (!any(vars %in% hznames)) {
      stop('names in formula do not match any horizon attributes', call. = FALSE)
    }
      
    
  } else {
    # no formula
    # slice to-depth of collection
    
    # optionally fill all gaps between min(x) --- [gaps] --- max(x)
    if (fill) {
      x <- fillHzGaps(x, flag = TRUE, to_top = min(x), to_bottom = max(x))
    }
    
    # all variables except profile ID, horizon ID, top, bottom
    vars <- hznames[-ids.top.bottom.idx]
    z <- NULL
  }
  
  
  # time to work with horizons 
  h <- horizons(x)
  
  # safely select variables
  h <- .data.frame.j(
    h, 
    col.names = c(hznames[ids.top.bottom.idx], vars)
  )
  
  # convert to DT
  if (!inherits(h, 'data.table')) {
    h <- as.data.table(h)
  }
  
  ## TODO: are keys worth the extra sorting / re-sorting?
  ##       no, probably not inherently worth it -AGB 2022/08/17
  # init keys, sorts the data on hzID (alpha-sort)
  # setkeyv(h, hzidn)
  # consider an index, seems to have no effect
  # setindexv(h, hzidn)
  
  ## expand 1 unit slices to max depth of each profile
  # TODO: this will have to be made more intelligent in the presence of overlap
  # mapply() call takes 1/4 of total time
  
  # safe wrapper to base::seq
  # NA in from/to (hz depths) not allowed
  .seq.safe <- function(from, to, by) {
    if (any(is.na(from)) | any(is.na(to))) {
      stop('corrupt horizonation, consider using `strict = TRUE`', call. = FALSE)
    }
    if (from > to) {
      toold <- to
      to <- from
      from <- toold
    }
    return(
      seq(from = from, to = to, by = abs(by))
    )
  }
  
  # from == to (0-thickness) horizons -- these should be removed -AGB 2022/08/17
  zthk <- h[[htb[1]]] == h[[htb[2]]]
  if (any(zthk, na.rm = TRUE)) {
    message("horizons with zero thickness have been omitted from results")
  }
  h.sub <- h[which(!zthk),]
  
  tops <- mapply(
    FUN = .seq.safe, 
    from = h.sub[[htb[1]]], 
    to = h.sub[[htb[2]]] - 1, 
    by = 1, 
    SIMPLIFY = FALSE
  )
  
  tops <- unlist(tops)
  bottoms <- tops + 1
  
  # expand slice IDs (horizon IDs)
  # these are used to join with horizons
  sliceIDs <- rep(
    h.sub[[hzidn]], 
    times = h.sub[[htb[2]]] - h.sub[[htb[1]]]
  )
  
  # assemble slice LUT for JOIN
  s <- data.table(
    sliceID = sliceIDs, 
    .sliceTop = tops[1:length(sliceIDs)], 
    .sliceBottom = bottoms[1:length(sliceIDs)]
  )
  
  # re-name for simpler JOIN
  names(s)[1] <- hzidn

  # FULL JOIN via fast data.table compiled code
  res <- merge(h, s, by = hzidn, all = TRUE, sort = FALSE)
  
  ## TODO: update to := syntax, but how does it work with with variables?
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reference-semantics.html
  # init unique horizon IDs
  res[['sliceID']] <- as.character(1:nrow(res))
  
  # copy old depths
  res[['.oldTop']] <- res[[htb[1]]]
  res[['.oldBottom']] <- res[[htb[2]]]
  
  # replace old depths with sliced depths
  res[[htb[1]]] <- res[['.sliceTop']]
  res[[htb[2]]] <- res[['.sliceBottom']]
  
  # cleanup
  res[['.sliceTop']] <- NULL
  res[['.sliceBottom']] <- NULL
  
  ## TODO: move z-index subset"up" to original sequence creation to save a lot of time
  if (!is.null(z)) {
    res <- res[which(res[[htb[1]]] %in% z), ]
  }
  
  # slice-wise "percent missing" calculation
  if (pctMissing) {
    
    # this is quite slow: DT -> DF -> matrix -> apply (8x longer)
    # res[['.pctMissing']] <- .pctMissing(res, vars)
    
    # native DT approach
    ## TODO: throws warning: https://github.com/ncss-tech/aqp/issues/115#issuecomment-785301769
    # count number of NA in vars, by row 
    # res[, .pctMissing := rowSums(is.na(res[, .SD, .SDcols = vars]))]
    
    res$'.pctMissing' <- NA
    set(res, j = '.pctMissing', value = rowSums(is.na(res[, .SD, .SDcols = vars])))
    
    # convert NA count to percentage
    # res[, .pctMissing := .pctMissing / length(vars)]
    set(res, j = '.pctMissing', value = res$.pctMissing / length(vars))
  }
  
  # only returning horizons as a data.frame
  if (!SPC) {
    return(as.data.frame(res))
  }
  
  ## TODO: conditionally
  # re-pack horizon data
  res <- as.data.frame(res)
  replaceHorizons(x) <- res
  
  # switch horizon ID to slice ID
  hzidname(x) <- 'sliceID'
  
  # final size
  f.size <- object.size(x)
  
  # object bloat factor
  OBF <- round(f.size / o.size, 1)
  metadata(x)$OBF <- OBF
  
  if (verbose) {
    # turned off by default; possibly informative 
    message(sprintf("Object Bloat Factor: %s", OBF))
  }
  
  return(x)
}

