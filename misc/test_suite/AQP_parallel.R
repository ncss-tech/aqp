## no cross-platform code yet.
## DEB

library(aqp)
require(parallel)

## does not work
cl <- makeCluster(4)
d <- ldply(1:100, function(i) random_profile(i), .parallel=TRUE)
stopCluster(cl)


