

## fairly generic, will be required by (at least):"
# * dice()
# * profile_compare()
# * slab()
# * spc2mpspline()

## TODO do we need all arguments to checkHzDepthLogic()?
# if using `...` then we need to explicitly "grab" byhz for later

#' @param x a `SoilProfileCollection` object
#' @param byhz logical, evaluate horizon depth logic at the horizon level (profile level if `FALSE`)
.HzDepthLogicSubset <- function(x, byhz = FALSE) {
  
  # additional arguments?
  hz.tests <- checkHzDepthLogic(x, fast = TRUE, byhz = byhz)
  
  # invalid data filtering modes:
  if(byhz) {
    # profile-level
    message("dropping horizons with invalid depth logic, see `metadata(x)$dice.removed.horizons`")
    
    # locate horizons to keep
    idx <- which(hz.tests$valid)
    
    # test for empty SPC
    if(length(idx) < 1) {
      stop('there are no valid profiles in this collection', call. = FALSE)
    }
    
    # keep track of invalid horizon IDs in @metadata
    bad.ids <- hz.tests[[hzidname(x)]][-idx]
    metadata(x)$dice.removed.horizons <- bad.ids
    
    # perform drop
    # this will trigger an error if SPC is corrupted (site w/o horizons)
    res <- try(
      replaceHorizons(x) <- horizons(x)[idx, ], 
      silent = TRUE
    )
    
    if(inherits(res, 'try-error')) {
      stop('removing horizons with invalid depth logic would corrupt `x`, use `byhz = FALSE`', call. = FALSE)
    }
    
  } else {
    # profile-level
    message("dropping profiles with invalid depth logic, see `metadata(x)$dice.removed.profiles`")
    
    # locate profiles to keep
    idx <- which(hz.tests$valid)
    
    # test for empty SPC
    if(length(idx) < 1) {
      stop('there are no valid profiles in this collection', call. = FALSE)
    }
    
    # keep track of invalid profile IDs in @metadata
    bad.ids <- hz.tests[[idname(x)]][-idx]
    metadata(x)$dice.removed.profiles <- bad.ids
    
    # perform drop
    x <- x[idx, ]
  }
  
  return(x)
  
}


# simpler, faster version of slice via `data.table` / FULL JOIN
# less robust to errors than current slice()
# slices entire profiles
# returns all depths / columns

#' @param x a `SoilProfileCollection` object
#' @param byhz logical, evaluate horizon depth logic at the horizon level (profile level if `FALSE`)
#' @param dropInvalid drop horizons (`byhz = TRUE`) or profiles (`byhz = FALSE`) with depth logic errors
#' @param SPC return the diced `SoilPrfolileCollection`, if `FALSE` a `data.frame` of horizon-level attributes
#' 
.dice <- function(x, byhz = TRUE, dropInvalid = TRUE, SPC = TRUE) {
  
  ## TODO:
  # * consider setindex() vs. setkey() <-- this sorts the data
  # * formula interface (nope)
  # * .pctMissing eval (write a new function)
  # * strictness of hz logic eval
  # * ERRORS on NA depths
  # * ERROR on top == bottom
  # * ERROR on bottom < top
  # * cannot use A/E type horizons (https://github.com/ncss-tech/aqp/issues/88)
  
  
  ## TODO: the following should be abstracted into the new function above ~ .HzDepthLogicSubset() for now
  # sanity check: profiles must pass all hz depth logic
  hz.tests <- checkHzDepthLogic(x, fast = TRUE, byhz = byhz)
  
  # depth logic errors
  if(any(!hz.tests$valid)) {
    
    # optionally drop invalid horizons / profiles
    if(dropInvalid) {
      
      # invalid data filtering modes:
      if(byhz) {
        # profile-level
        message("dropping horizons with invalid depth logic, see `metadata(x)$dice.removed.horizons`")
        
        # locate horizons to keep
        idx <- which(hz.tests$valid)
        
        # test for empty SPC
        if(length(idx) < 1) {
          stop('there are no valid profiles in this collection', call. = FALSE)
        }
        
        # keep track of invalid horizon IDs in @metadata
        bad.ids <- hz.tests[[hzidname(x)]][-idx]
        metadata(x)$dice.removed.horizons <- bad.ids
        
        # perform drop
        # this will trigger an error if SPC is corrupted (site w/o horizons)
        res <- try(
          replaceHorizons(x) <- horizons(x)[idx, ], 
          silent = TRUE
        )
        
        if(inherits(res, 'try-error')) {
          stop('removing horizons with invalid depth logic would corrupt `x`, use `byhz = FALSE`', call. = FALSE)
        }
        
      } else {
        # profile-level
        message("dropping profiles with invalid depth logic, see `metadata(x)$dice.removed.profiles`")
        
        # locate profiles to keep
        idx <- which(hz.tests$valid)
        
        # test for empty SPC
        if(length(idx) < 1) {
          stop('there are no valid profiles in this collection', call. = FALSE)
        }
        
        # keep track of invalid profile IDs in @metadata
        bad.ids <- hz.tests[[idname(x)]][-idx]
        metadata(x)$dice.removed.profiles <- bad.ids
        
        # perform drop
        x <- x[idx, ]
      }
      
    } else {
      # cannot work with invalid hz logic
      stop('invalid horizon depth logic detected', call. = FALSE)  
    }
  }
  
  ## extract pieces
  h <- horizons(x)
  idn <- idname(x)
  hzidn <- hzidname(x)
  htb <- horizonDepths(x)
  
  ## `h` could be a data.table object
  h <- as.data.table(h)
  
  ## TODO: are keys worth the extra sorting / re-sorting?
  # init keys, sorts the data on hzID (alpha-sort)
  # setkeyv(h, hzidn)
  # consider and index, seems to have no effect
  # setindexv(h, hzidn)
  
  ## mapply() call takes 1/2 of total time
  ## consider custom function
  # expand 1 unit slices to max depth of each profile
  # NA in hz depths or 0-thickness horizons are not allowed
  tops <- mapply(
    FUN = seq, 
    from = h[[htb[1]]], 
    to = h[[htb[2]]] - 1, 
    by = 1, 
    SIMPLIFY = FALSE
  )
  
  tops <- unlist(tops)
  bottoms <- tops + 1
  
  # expand slice IDs (horizon IDs)
  # these are used to join with horizons
  sliceIDs <- rep(
    h[[hzidn]], 
    times = h[[htb[2]]] - h[[htb[1]]]
  )
  
  # convert to v
  
  # assemble slice LUT for JOIN
  s <- data.table(
    sliceID = sliceIDs, 
    .sliceTop = tops, 
    .sliceBottom = bottoms
  )
  
  # re-name for simpler JOIN
  names(s)[1] <- hzidn
  
  ## MAYBE
  # perform subsetting of depths / variables using `fm` if provided
  # s[.sliceTop %in% depthvec]
  
  ## TODO: are keys worth the extra sorting / re-sorting?
  # note: sorts data
  # setkeyv(s, hzidn)
  # consider and index, seems to have no effect
  # setindexv(s, hzidn)
  
  # FULL JOIN via fast data.table compiled code
  # using index (?)
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
  
  # only returning horizons as a data.frame
  if(!SPC) {
    return(as.data.frame(res))
  }
  
  # re-pack horizon data
  res <- as.data.frame(res)
  replaceHorizons(x) <- res
  
  # switch horizon ID to slice ID
  hzidname(x) <- 'sliceID'
  
  return(x)
}

