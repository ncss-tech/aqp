library(aqp)
library(plyr)
library(lattice)
library(data.table)
library(reshape)

d <- ldply(1:5000, random_profile, n=c(6, 7, 8), n_prop=5)
depths(d) <- id ~ top + bottom
d$group <- sample(letters[1:10], size=length(d), replace=TRUE)


# basic quantile evaluation, better for large datasets
dt.slab.fun.numeric.fast <- function(values) {
  q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  res <- quantile(values, probs=q.probs, na.rm=TRUE)
  names(res) <- paste('p.q', round(q.probs * 100), sep='')
  return(res)
  
  # manual re-structuring of NA results for DT-compatibility
#   if(any(is.na(res))) {
#     return(as(rep(NA, 5), class(values))) 
#   } 
#   
#   else { 
#     return(res)
#   }
  
}


## will this work with NA-removed ?
cf <- function(i) {
  length(na.omit(i)) / length(i)
}



# slab2 is about 25%-50% faster, but at the expense of more complex slab functions
system.time(a.1 <- slab(d, fm=group ~ p1 + p2 + p3 + p4 + p5, slab.fun=dt.slab.fun.numeric.fast))
system.time(a.2 <- slab2(d, fm=group ~ p1 + p2 + p3 + p4 + p5, slab.fun=dt.slab.fun.numeric.fast))


## progress:
# 1. slab2() doesn't apply slab.fun to NA... this will break CF estimates
# 2. missing contributing fraction
# 3. need to test categorical and other slab functions
# 4. need to implement weighted functions
# 5. how to pass-in additional arguments?


## ideas:
# http://stackoverflow.com/questions/12399592/na-in-data-table


