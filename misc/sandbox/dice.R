
library(aqp)
# library(data.table)

d <- lapply(1:5000, random_profile, n=c(6, 7, 8), n_prop=5, method='LPP')
d <- do.call('rbind', d)
d$id <- as.character(d$id)
depths(d) <- id ~ top + bottom
d$group <- factor(sample(letters[1:10], size=length(d), replace=TRUE))


# simpler, faster version of slice via FULL JOIN
dice <- function(x) {
  
  ## extract pieces
  h <- horizons(x)
  idn <- idname(x)
  htb <- horizonDepths(x)
  
  # slice ID, expand rows via JOIN
  h$.sliceID <- sprintf('%s_%s_%s', h[[idn]], h[[htb[1]]], h[[htb[2]]] )
  
  # expand 1 unit slices
  tops <- mapply(FUN = seq, from=h[[htb[1]]], to=h[[htb[2]]] - 1, by = 1)
  bottoms <- mapply(FUN = seq, from=h[[htb[1]]] + 1, to=h[[htb[2]]], by = 1)
  
  # expand slice IDs
  sliceIDs <- sprintf('%s_%s_%s', h[[idn]], h[[htb[1]]], h[[htb[2]]] )
  sliceIDs <- rep(sliceIDs, times=h[[htb[2]]] - h[[htb[1]]])
  
  # assemble for JOIN
  s <- data.frame(.sliceID=sliceIDs, .sliceTop=unlist(tops), .sliceBottom=unlist(bottoms), stringsAsFactors = FALSE)
  
  # full JOIN
  res <- merge(h, s, by='.sliceID', all.x=TRUE, all.y=TRUE, sort=FALSE)
  
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
  res[['.sliceID']] <- NULL
  
  # pack back into SPC
  horizons(x) <- res
  
  return(x)
}



z <- dice(d[1:10, ])

plot(z, color='p2')
