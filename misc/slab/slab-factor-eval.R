
library(aqp)


## pre-cached/subset data
# all VALID
x <- readRDS('clarksville-pedons-final.rds')


# compute slice-wise probability: slice-wise P always sum to 1
a.1 <- slab(x, ~ genhz, cpm = 1)

a.2 <- slab(x, ~ genhz, cpm = 2)
 
# saveRDS(a, file = 'slab-factor-1x-cpm2.rds')

# saveRDS(a, file = 'e:/temp/slab-factor-2x.rds')
