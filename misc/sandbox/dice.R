
library(aqp)
library(data.table)

## indexes vs. keys
# See the vignette("datatable-secondary-indices-and-auto-indexing") for more details.

# ~ 10 seconds for 10k profiles
# much faster to generate as DF, then promote to SPC at the end
d <- lapply(1:10000, random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = FALSE)

# much faster: rbind + init SPC after making individual profiles
d <- do.call('rbind', d)

d$id <- as.character(d$id)
depths(d) <- id ~ top + bottom

# fake group
site(d)$group <- factor(sample(letters[1:10], size = length(d), replace =TRUE))

plotSPC(d[1:10, ], color = 'p1')


## DT full outer join ideas
# https://stackoverflow.com/questions/15170741/how-does-one-do-a-full-join-using-data-table
# 

## address TODO and major design questions:
# https://github.com/ncss-tech/aqp/issues/115


# simpler, faster version of slice via data.table / FULL JOIN
# less robust to errors than current slice()
# slices entire profiles
# returns all columns

#' @param x a `SoilProfileCollection` object
#' @param dropInvalid drop profiles with horizon errors
#' @param just.the.data return only the horizon data, for backwards compatibility
#' 
dice <- function(x, dropInvalid = TRUE, just.the.data = FALSE) {
  
  ## TODO:
  # * consider setindex() vs. setkey() <-- this sorts the data
  # * formula interface
  # * .pctMissing eval
  # * return as DF vs. SPC
  # * strictness of hz logic eval
  # * ERRORS on NA depths
  # * ERROR on top == bottom
  # * ERROR on bottom < top
  # * cannot use A/E type horizons (https://github.com/ncss-tech/aqp/issues/88)
  
  # sanity check: profiles must pass all hz depth logic
  hz.tests <- checkHzDepthLogic(x, fast = TRUE)
  
  if(any(!hz.tests$valid)) {
    
    if(dropInvalid) {
      message('dropping profiles with invalid horizon logic')
      idx <- which(hz.tests$valid)
      x <- x[idx, ]
      
      # test for empty SPC
      if(length(idx) < 1) {
        stop('there are no valid profiles in this collection', call. = FALSE)
      }
      
    } else {
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
  tops <- mapply(FUN = seq, from = h[[htb[1]]], to = h[[htb[2]]] - 1, by = 1, SIMPLIFY = FALSE)
  tops <- unlist(tops)
  bottoms <- tops + 1
  
  # expand slice IDs (horizon IDs)
  sliceIDs <- rep(h[[hzidn]], times = h[[htb[2]]] - h[[htb[1]]])
  
  # assemble slice LUT for JOIN
  s <- data.table(
    sliceID = sliceIDs, 
    .sliceTop = tops, 
    .sliceBottom = bottoms
  )
  
  # re-name for simpler JOIN
  names(s)[1] <- hzidn
  
  ## TODO: are keys worth the extra sorting / re-sorting?
  # note: sorts data
  # setkeyv(s, hzidn)
  # consider and index, seems to have no effect
  # setindexv(s, hzidn)
  
  # FULL JOIN via fast data.table compiled code
  # using keys (index)
  res <- merge(h, s, by = hzidn, all = TRUE, sort = FALSE)
  
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
  
  # only returning horizons as a data.frame
  if(just.the.data) {
    return(as.data.frame(res))
  }
  
  # re-pack horizon data
  res <- as.data.frame(res)
  replaceHorizons(x) <- res
  
  # switch horizon ID to slice ID
  hzidname(x) <- 'sliceID'
  
  return(x)
}


z <- dice(d[1:2, ])

par(mar = c(0,1,3,1))
# supress hz names
# strange legend, due to character representation of integers
plotSPC(z[2, ], color = 'hzID', name = NA, divide.hz = FALSE)



## introduce horizonation errors

z <- d[1:10, ]
z$bottom[2] <- NA
z$top[20] <- z$bottom[20]

z$bottom[32] <- 15
z$bottom[5]
z$top[6] <- 95

zz <- dice(z)

plotSPC(zz, color = 'hzID', name = NA, divide.hz = FALSE)


## current slice()
# 10k profiles: 15 seconds (home MacOS)
# 100k profiles: 266 seconds
system.time(s <- slice(d, 0:100 ~ .))

## 1x mapply, merge.data.table
# 10k profiles: 3 seconds (home MacOS)
# 100k profiles: 42 seconds
system.time(s <- dice(d))


## profile

# get.slice() wastes a lot of time
pp.slice <- profvis(s <- slice(d, 0:100 ~ .))

# most time spent: setkey + mapply + setkey
pp.dice <- profvis(s <- dice(d))







