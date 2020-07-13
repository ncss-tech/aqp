
library(aqp)
library(data.table)
library(profvis)


# ~ 10 seconds for 10k profiles
# much faster to generate as DF, then promote to SPC at the end
d <- lapply(1:10000, random_profile, n=c(6, 7, 8), n_prop=5, method='LPP', SPC=FALSE)

# much faster: rbind + init SPC after making individual profiles
d <- do.call('rbind', d)

d$id <- as.character(d$id)
depths(d) <- id ~ top + bottom

# fake group
d$group <- factor(sample(letters[1:10], size=length(d), replace=TRUE))

## DT full outer join ideas
# https://stackoverflow.com/questions/15170741/how-does-one-do-a-full-join-using-data-table
# 

## address TODO and major design questions:
# https://github.com/ncss-tech/aqp/issues/115


# simpler, faster version of slice via data.table / FULL JOIN
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
  # * cannot use A/E type horizons (https://github.com/ncss-tech/aqp/issues/88)
  
  ## extract pieces
  h <- horizons(x)
  idn <- idname(x)
  hzidn <- hzidname(x)
  htb <- horizonDepths(x)
  
  ## `h` could be a data.table object
  h <- as.data.table(h)
  setkeyv(h, hzidn)
  
  ## mapply() call takes 1/2 of total time
  ## consider custom function
  # expand 1 unit slices
  tops <- mapply(FUN = seq, from=h[[htb[1]]], to=h[[htb[2]]] - 1, by = 1, SIMPLIFY=FALSE)
  tops <- unlist(tops)
  bottoms <- tops + 1
  
  # expand slice IDs (horizon IDs)
  sliceIDs <- rep(h[[hzidn]], times=h[[htb[2]]] - h[[htb[1]]])
  
  ## TODO: this will eventually be a data.table object
  # assemble for JOIN
  s <- data.frame(sliceID=sliceIDs, .sliceTop=tops, .sliceBottom=bottoms, stringsAsFactors = FALSE)
  # re-name for simpler JOIN
  names(s)[1] <- hzidn
  # init / index data.table
  s <- as.data.table(s)
  setkeyv(s, hzidn)
  
  # FULL JOIN via fast data.table compiled code
  res <- merge.data.table(h, s, by=hzidn, all=TRUE, sort=FALSE)
  
  ## TODO: update to := syntax, but how does it work with with variables?
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reference-semantics.html
  # init unique horizon IDs
  res[['sliceID']] <- 1:nrow(res)
  
  # copy old depths
  res[['.oldTop']] <- res[[htb[1]]]
  res[['.oldBottom']] <- res[[htb[2]]]
  
  # replace old depths with sliced depths
  res[[htb[1]]] <- res[['.sliceTop']]
  res[[htb[2]]] <- res[['.sliceBottom']]
  
  # cleanup
  res[['.sliceTop']] <- NULL
  res[['.sliceBottom']] <- NULL
  
  # pack back into SPC by replacement of @horizons
  # otherwise the setter will attempt to join the data on hzidname
  res <- as.data.frame(res)
  slot(x, 'horizons') <- res
  
  # switch horizon ID to slice ID
  hzidname(x) <- 'sliceID'
  
  return(x)
}


z <- dice(d[1:2, ])

par(mar=c(0,1,3,1))
# supress hz names
# strange legend, due to character representation of integers
plotSPC(z, color='hzID', name=NA, divide.hz = TRUE)

## current slice()
# 10k profiles: 22 seconds 
system.time(s <- slice(d, 0:100 ~ .))

## 1x mapply, merge.data.table
# 10k profiles: 4 seconds
system.time(s <- dice(d))


## profile

# get.slice() wastes a lot of time
pp.slice <- profvis(s <- slice(d, 0:100 ~ .))

# most time spent: setkey + mapply + setkey
pp.dice <- profvis(s <- dice(d))


## introduce horizonation errors

z <- d[1:10, ]
z$bottom[2] <- NA
z$top[20] <- z$bottom[20]

z$bottom[32] <- 15
z$bottom[5]
z$top[6] <- 95

checkHzDepthLogic(z)

dice(z)





