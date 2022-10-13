
library(aqp)


## pre-cached/subset data
# all VALID
x <- readRDS('misc/slab/clarksville-pedons-final.rds')

# keep track of generalized horizon names for later
hz.names <- levels(x$genhz)

# slice out color and horizon name into 1cm intervals: no aggregation
max.depth <- 180
slice.resolution <- 1
slice.vect <- seq(from = 0, to = max.depth, by = slice.resolution)
s <- slice(x, slice.vect ~ genhz)

# convert horizon name to factor
s$genhz <- factor(s$genhz, levels = hz.names)

# compute slice-wise probability: slice-wise P always sum to 1
a <- slab(x, ~ genhz, cpm = 1)
 
# saveRDS(a, file = 'misc/slab/slab-factor-1x.rds')
# saveRDS(a, file = 'misc/slab/slab-factor-2x.rds')

a.1 <- slab(x, ~ genhz, cpm = 1)
a.2 <- slab(x, ~ genhz, cpm = 2)
# saveRDS(a.1, file = 'misc/slab/slab-factor-1x-cpm1.rds')
# saveRDS(a.2, file = 'misc/slab/slab-factor-1x-cpm2.rds')
saveRDS(a.1, file = 'misc/slab/slab-factor-2x-cpm1.rds')
saveRDS(a.2, file = 'misc/slab/slab-factor-2x-cpm2.rds')

# saveRDS(a, file = 'misc/slab/slab-factor-1x-cpm2.rds')
# saveRDS(a, file = 'misc/slab/slab-factor-2x.rds')

library(daff)

d1 <- readRDS('misc/slab/slab-factor-1x.rds')
d2 <- readRDS('misc/slab/slab-factor-2x.rds')
diff_data(d1,d2) |> 
  render_diff()
