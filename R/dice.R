
## https://github.com/ncss-tech/aqp/issues/115

## TODO: 
##   * DT full outer join ideas
##     https://stackoverflow.com/questions/15170741/how-does-one-do-a-full-join-using-data-table

setGeneric("dice", function(x,
                            fm = NULL,
                            SPC = TRUE,
                            pctMissing = FALSE,
                            fill = FALSE,
                            strict = TRUE,
                            byhz = TRUE,
                            verbose = FALSE)
  standardGeneric("dice"))

.dice <- function(x,
                  fm = NULL,
                  SPC = TRUE,
                  pctMissing = FALSE,
                  fill = FALSE,
                  strict = TRUE,
                  byhz = TRUE,
                  verbose = FALSE) {
  
  # sacrifice to R CMD check spirits
  .pctMissing <- NULL
  
  # find / flag / remove invalid profiles or horizons
  # this will generate an error if there are no valid profiles remaining
  if (strict) {
    x <- HzDepthLogicSubset(x, byhz = byhz)
    
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
    # z-index
    z <- as.numeric(eval(parse(text = fm[[1]])))
    
    # RHS: variable names
    vars <- fm[[2]] 
    
    # check for bogus left/right side problems with the formula
    if (any(z < 0) | any(is.na(z))) {
      stop('z-index must be >= 0', call. = FALSE)
    }
    
    # check for integer / numeric slices
    if (!inherits(z,  c('numeric','integer'))) {
      stop('z-index must be either numeric or integer', call. = FALSE)
    }
    
    # if z-index is missing set to NULL for later on
    if (length(z) == 0) {
      z <- NULL
    } else {
      # z-index is specified
      # note z-index defines slice tops, lower limit is (z + 1)
      # must fill from min(z) --- [gaps] --- max(z) + 1
      x <- fillHzGaps(x, flag = TRUE, to_top = min(z), to_bottom = max(z) + 1)
      
      # NB: fillHzGaps will recalculate/reset hzID, so update the local var!
      hzidn <- "hzID"
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
  
  hmnames <- .hzMetadataNames(x, depths = TRUE)
  newhm <- hmnames[which(!hmnames %in% vars)]
  
  # safely select variables
  h <- .data.frame.j(h, col.names = c(newhm, vars))
  
  # convert to DT, as needed
  if (!inherits(h, 'data.table')) {
    h <- as.data.table(h)
  }
  
  ## TODO: this needs to be integrated / coordinated with 
  #        strict = TRUE, HzDepthLogicSubset() 
  
  ## TODO: how will this affect the FULL JOIN below if subset?
  
  # flag 0-thickness horizons
  zthk <- h[[htb[1]]] == h[[htb[2]]]
  if (any(zthk, na.rm = TRUE)) {
    message("horizons with zero thickness have been omitted from results")
  }
  
  # remove any 0-thickness horizons
  h.sub <- h[which(!zthk), ]
  
  # expand 1 unit slices to max depth of each profile
  #  calculate sequence of top depths of slices using data.table j index
  tops <- h.sub[, 
                list(
                  # iterate over horizons in each profile
                  sapply(seq_len(.N), function(i) {
                    
                    # calculate the number of slices in source horizon
                    j <- seq_len(abs(.SD[[htb[2]]][i] - .SD[[htb[1]]][i]))
                    
                    # when top and bottom are equal, no slice, return 0-length 
                    if (length(j) == 0) 
                      return(j)
                    
                    # calculate sequence from input top depth, minus 1
                    list(.SD[[htb[1]]][i] + j - 1)
                  })
                ), 
                # iterate over profiles in the collection (by idname)
                by = c(idn), 
                
                # .SD contains only ID + depths (more efficient)
                .SDcols = htb]
  
  tops <- unlist(tops$V1)
  bottoms <- tops + 1
  
  # expand slice IDs (horizon IDs); used to join with horizons
  sliceIDs <- rep(
    h.sub[[hzidn]], 
    times = abs(h.sub[[htb[2]]] - h.sub[[htb[1]]])
  )
  
  # assemble slice LUT for JOIN
  s <- data.table(
    sliceID = sliceIDs, 
    .sliceTop = tops,
    .sliceBottom = bottoms
  )
  
  # re-name for simpler JOIN
  names(s)[1] <- hzidn
  
  # FULL JOIN via fast data.table compiled code
  res <- merge(h, s, by = hzidn, all = TRUE, sort = FALSE)
  
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
  
  ## TODO: perform z-index subset before slice sequence creation to save a lot of time
  if (!is.null(z)) {
    # selection via top depth within z-index
    res <- res[which(res[[htb[1]]] %in% z), ]
  }
  
  # slice-wise "percent missing" calculation
  ## NOTE: this does not exclude horizon metadata columns like hzname, hztexture
  ##       -> unless formula RHS is specified, but not with ~ .
  if (pctMissing) {
    
    # this is quite slow: DT -> DF -> matrix -> apply (8x longer)
    # res[['.pctMissing']] <- .pctMissing(res, vars)
    
    # hybrid base + DT approach
    res$'.pctMissing' <- NA
    # count number of NA in vars, by row
    set(res, j = '.pctMissing', value = rowSums(is.na(res[, .SD, .SDcols = vars])))
    
    # convert NA count to percentage
    set(res, j = '.pctMissing', value = res$.pctMissing / length(vars))
  }
  
  # ensure sliced horizons are in ID/top order
  res <- res[order(res[[idname(x)]], res[[horizonDepths(x)[1]]]), ]
  
  # only returning horizons as a data.frame
  if (!SPC) {
    return(as.data.frame(res))
  }
  
  # re-pack horizon data
  res <- .as.data.frame.aqp(res, aqp_df_class(x))
  
  # this will fail if strict = FALSE, and sub-setting resulted in corrupt SPC
  .condition <- try(replaceHorizons(x) <- res, silent = TRUE)
  
  # gracefully fail
  if (inherits(.condition, 'try-error')) {
    stop('SPC object corruption, please specify `strict = TRUE`', call. = FALSE)
  }
  
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

## TODO: allow the use of site data (PSC etc.) to determine the z-slice

#' @title Efficient Slicing of `SoilProfileCollection` Objects
#' 
#' @description Cut ("dice") soil horizons into 1-unit thick slices. This function replaces `aqp::slice()`, which will be deprecated in aqp 2.0.
#'
#'
#' @param x a `SoilProfileCollection` object
#' 
#' @param fm optional `formula` describing top depths and horizon level attributes to include: `integer.vector ~ var1 + var2 + var3` or `integer.vector ~ .` to include all horizon level attributes. Specification of `integer.vector` forces `fill = TRUE`. When `NULL` profiles are "diced" to depth and results will include all horizon level attributes. Note on interpretation of `integer.vector` (slice tops)
#' 
#' @param SPC return the diced `SoilPrfolileCollection`, if `FALSE` a `data.frame` of horizon-level attributes
#' 
#' @param pctMissing compute "percent missing data" by slice (when `TRUE` expect 6-8x longer run time)
#' 
#' @param fill logical, fill with empty placeholder horizons in gaps within profiles, and/or, above/below interval specified in `fm`. Automatically set to `TRUE` when LHS of `fm` is specified. Backwards compatibility with `slice` is maintained by setting `fill = TRUE` with or without `fm`.
#' 
#' @param strict perform horizon depth logic checking / flagging / removal
#' 
#' @param byhz Evaluate horizon depth logic at the horizon level (`TRUE`) or profile level (`FALSE`). Invalid depth logic invokes `HzDepthLogicSubset` which removes offending profiles or horizon records.
#' 
#' @param verbose Print information about object size/memory usage. Default: `FALSE`
#' 
#' @details For large and potentially messy collections that may include missing horizon depth logic errors, consider using `repairMissingHzDepths()` before `dice()`. Consider using `accumulateDepths()` before invoking `dice()` on collections that may contain old-style O horizon notation (e.g. 5-0cm).
#' 
#' @aliases dice
#' 
#' @seealso [repairMissingHzDepths()], [accumulateDepths()], [fillHzGaps()]
#' 
#' @return a `SoilProfileCollection` object, or `data.frame` when `SPC = FALSE`
#' 
#' @author D.E. Beaudette and A.G. Brown 
#' 
#' @export
setMethod(f = 'dice', signature(x = 'SoilProfileCollection'), .dice)
