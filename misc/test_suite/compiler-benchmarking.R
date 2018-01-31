##
## 01-06-2012: not much improvement...
##

library(aqp)
library(plyr)
library(compiler)
library(rbenchmark)

ppc <- cmpfun(pc)

d <- ldply(1:1000, random_profile, n=c(3, 5, 6), n_prop=5, method='LPP')

benchmark(original=pc(d, vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0), compiled=ppc(d, vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0), columns=c("test", "replications", "elapsed", "relative"), order="relative", replications=10)


test replications elapsed relative
1 original           10  277.00 1.000000
2 compiled           10  285.37 1.030217



depths(d) <- id ~ top + bottom
sf <- cmpfun(slice.fast)


benchmark(original=slice.fast(d, 1:100 ~ p1 + p2 + p3), compiled=sf(d, 1:100 ~ p1 + p2 + p3), columns=c("test", "replications", "elapsed", "relative"), order="relative", replications=10)

test replications elapsed relative
1 original           10   27.72 1.000000
2 compiled           10   27.97 1.009019