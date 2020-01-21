
library(aqp)
library(data.table)

d <- lapply(1:50000, random_profile, n=c(6, 7, 8), n_prop=5, method='LPP')
d <- do.call('rbind', d)
d$id <- as.character(d$id)
depths(d) <- id ~ top + bottom
d$group <- factor(sample(letters[1:10], size=length(d), replace=TRUE))

## DT full outer join ideas
# https://stackoverflow.com/questions/15170741/how-does-one-do-a-full-join-using-data-table
# 

## TODO convert to profileApply() invocation
## general-purpose hz depth logic check + fix
checkHzDepthLogic <- function(x) {
  
  ## profileApply()
  
  h <- horizons(x)
  
  # bottom depth < top depth?
  idx.1 <- which(h$bottom < h$top)
  
  # bottom depth == top depth
  idx.2 <- which(h$top == h$bottom)
  
  # NA depths
  idx.3 <- which(is.na(h$top) | is.na(h$bottom))
  
  # bottom != next top
  idx.4 <- which(h$top[-1] != h$bottom[-nrow(h)])
  
  if(length(idx.1) > 0 | length(idx.2) > 0 | length(idx.3 > 3)) {
    warning('depth logic errors present')
  }
  
  
  ## TODO: return NULL if no errors
  # convert horizon indexes into profile_ids
  res <- list(
    depthLogicError=h[[idname(x)]][idx.1], 
    sameDepth=h[[idname(x)]][idx.2], 
    missingDepth=h[[idname(x)]][idx.3],
    overlapOrGap=h[[idname(x)]][idx.4]
    )
  
  return(res)
}


# simpler, faster version of slice via FULL JOIN
# less robust to errors than current slice()
dice <- function(x) {
  
  ## TODO:
  # * formula interface
  # * .pctMissing eval
  # * return as DF vs. SPC
  # * strictness of hz logic eval
  # * ERRORS on NA depths
  # * ERROR on top == bottom
  # * ERROR on bottom < top
  
  ## extract pieces
  h <- horizons(x)
  idn <- idname(x)
  htb <- horizonDepths(x)
  
  ## `h` will eventually be a data.table object
  h <- as.data.table(h)
  setkeyv(h, hzidname(x))
  
  ## mapply() call takes 1/2 of total time
  ## consider custom function
  # expand 1 unit slices
  tops <- mapply(FUN = seq, from=h[[htb[1]]], to=h[[htb[2]]] - 1, by = 1, SIMPLIFY=FALSE)
  tops <- unlist(tops)
  bottoms <- tops + 1
  
  # expand slice IDs (horizon IDs)
  sliceIDs <- rep(h[[hzidname(x)]], times=h[[htb[2]]] - h[[htb[1]]])
  
  ## TODO: this will eventually be a data.table object
  # assemble for JOIN
  s <- data.frame(.sliceID=sliceIDs, .sliceTop=tops, .sliceBottom=bottoms, stringsAsFactors = FALSE)
  # re-name for simpler JOIN
  names(s)[1] <- hzidname(x)
  # init / index data.table
  s <- as.data.table(s)
  setkeyv(s, hzidname(x))
  
  ## TODO: utilize fast joins via data.table objects with keys SET
  # full JOIN
  res <- merge.data.table(h, s, by=hzidname(x), all=TRUE, sort=FALSE)
  
  # copy old horizon IDs
  res[['.oldHzID']] <- res[[hzidname(x)]]
  # init unique horizon IDs
  res[[hzidname(x)]] <- 1:nrow(res)
  
  # copy old depths
  res[['.oldTop']] <- res[[htb[1]]]
  res[['.oldBottom']] <- res[[htb[2]]]
  
  # replace old depths with sliced depths
  res[[htb[1]]] <- res[['.sliceTop']]
  res[[htb[2]]] <- res[['.sliceBottom']]
  
  # cleanup
  res[['.sliceTop']] <- NULL
  res[['.sliceBottom']] <- NULL
  
  # pack back into SPC
  res <- as.data.frame(res)
  horizons(x) <- res
  
  return(x)
}


z <- dice(d[1:20, ])

plot(z, color='p2')

# 87 seconds: current slice()
system.time(s <- slice(d, 0:100 ~ .))

# 39 seconds: 2x mapply, merge.data.frame
# 12 seconds: 1x mapply, merge.data.table
system.time(s <- dice(d))



z <- d[1, ]
z$bottom[2] <- NA

dice(z)





