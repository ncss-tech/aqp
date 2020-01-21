
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

# simpler, faster version of slice via FULL JOIN
dice <- function(x) {
  
  ## TODO:
  # * formula interface
  # * .pctMissing eval
  # * return as DF vs. SPC
  # * strictness of hz logic eval
  # * test: what happens with bogus horizonation?
  
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


z <- dice(d[1, ])

plot(z, color='p2')

# 87 seconds: current slice()
system.time(s <- slice(d, 0:100 ~ .))

# 39 seconds: 2x mapply, merge.data.frame
# 12 seconds: 1x mapply, merge.data.table
system.time(s <- dice(d))


