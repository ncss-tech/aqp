
library(aqp)
library(data.table)
library(microbenchmark)

# inspecting enhancements to .coalesce.idx (workhorse for integrity functions)
dat <- data.table(id=c(1,1,1,2,1,4,5,6))
aqp:::.coalesce.idx(dat[["id"]])

# one million IDs (similar to e.g. NASIS peiids or cokeys )
test.ids <- (as.character(round(runif(1e4,100000,1E7))))

# before data.table extensions
.coalesce.idx.old <- function(x) {
  lut <- x
  if(inherits(x, 'character'))
    lut <- as.integer(factor(x, ordered = TRUE))
  x[which(diff(c(0,lut)) != 0)]
}

# benchmark one with data.table v.s. old
microbenchmark::microbenchmark(.coalesce.idx.old(test.ids),
                               aqp:::.coalesce.idx(test.ids))

profvis::profvis(aqp:::.coalesce.idx(test.ids))
