# benchmark "old" way
library(aqp)
library(microbenchmark)

# number of test profiles
n <- 1000

# create random 'large' SPC (enough to go slow iterating over each element)
rnd <- aqp::combine(lapply(1:n, random_profile, SPC = TRUE))

# create set of n pairs of random top and bottom depths
rnd_dep <- data.frame(top = round(runif(n, 10, 20)),
                      bottom = round(runif(n, 30, 40)))

# n=1000 pedons benchmark; NEW ~300x faster
bench::mark(OLD = { aqp::combine(lapply(1:nrow(rnd_dep), function(i) {
                    glom(rnd[i, ], rnd_dep$top[i], rnd_dep$bottom[i])
                  }))$top 
                  }, 
            NEW = { 
                    glom(rnd[1:n, ], rnd_dep$top, rnd_dep$bottom)$top 
                  }, iterations = 10)

# expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time    
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>         
#   1 OLD       11.1s    14.2s    0.0737        NA     1.95    10   264      2.26m 
#   2 NEW      35.4ms   39.7ms   24.2           NA     2.42    10     1   414.05ms

# benchmark with "typical" SPC, loafercreek, n=106; NEW ~25% faster
data(loafercreek, package='soilDB')

# calculate argillic boundaries using profileApply (iterating over individual profiles)
argillic <- profileApply(loafercreek, function(x) data.frame(id = profile_id(x), t(getArgillicBounds(x))),
                         frameify = TRUE)

# old: glom boundaries iterating over individual profiles, then re-combine into single SPC
microbenchmark( 
  aqp::combine(lapply(1:nrow(argillic), function(i) 
    glom(loafercreek[i,], argillic$ubound[i], argillic$lbound[i])))
)
# min lq   mean median uq  max neval
# 86 89 174.97     90 92 7299   100

# new: process with vectorized glom
microbenchmark(
  glom(loafercreek, argillic$ubound, argillic$lbound)
)
# min lq   mean median uq  max neval
# 86 89 131.81     90 92 3315   100


# multiglom data.table testing

# all "argillic horizons" in loafercreek (via 't' subscript)
plot(glom(loafercreek, argillic$ubound, argillic$lbound))

p <- loafercreek[3:5,]
z1 <- c(10,20,30)
z2 <- c(20,30,40)
modality <- "all"
truncate <- TRUE
as.list <- FALSE

# # internal method comparisons
# .multiglom <- aqp:::.multiglom
# .multiglomDT <- aqp:::.multiglomDT

# # both are vectorized over p
# expect_equal(.multiglom(p, z1[1], z2[1]),
#              .multiglomDT(p, z1[1], z2[1]))
# 
# # .multiglomDT is vectorized over z1 and z2 as well as p
# expect_equal(c(.multiglom(p[1, ], z1[1], z2[1]),
#                .multiglom(p[2, ], z1[2], z2[2]),
#                .multiglom(p[3, ], z1[3], z2[3])),
#              .multiglomDT(p, z1, z2))

# alternating z1/z2
z1 <- c(rep(c(0,55), 5))[1:9]
z2 <- c(rep(c(10,60), 5))[1:9]

# sp1 dataset
data(sp1)
depths(sp1) <- id ~ top + bottom

# profiles colored in with "prop" alternate [0-10] and [55-60]
par(mar = c(1, 1, 3, 1))
plot(sp1, max.depth = 60)
plot(glom(sp1, z1, z2, truncate = TRUE),
     max.depth = 60,
     add = TRUE,
     color = "prop",
     name = NA)
