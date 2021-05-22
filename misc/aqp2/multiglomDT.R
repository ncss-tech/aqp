# benchmark "old" way
library(aqp)
library(microbenchmark)

rnd <- aqp::combine(lapply(1:10000, random_profile, SPC=TRUE))
n = 1000

rnd_dep <- data.frame(top = round(runif(n, 10, 20)),
 bottom = round(runif(n, 30, 40)))

system.time(aqp::combine(lapply(
  1:nrow(rnd_dep), \(i) {
    glom(rnd[i, ], rnd_dep$top[i], rnd_dep$bottom[i])
  }
)))

system.time(glom(rnd[1:n, ], rnd_dep$top, rnd_dep$bottom))

data(loafercreek, package='soilDB')

argillic <- profileApply(loafercreek, 
                         \(x) data.frame(id = profile_id(x),
                                         t(getArgillicBounds(x))),
                         frameify = TRUE)

microbenchmark(\(x) 
  aqp::combine(lapply(1:nrow(argillic), function(i) 
    glom(loafercreek[i,], argillic$ubound[i], argillic$lbound[i])))
)

microbenchmark(\(x) 
  glom(loafercreek, argillic$ubound, argillic$lbound)
)

# multiglom data.table testing

plot(glom(loafercreek, argillic$ubound, argillic$lbound))

p <- loafercreek[3:5,]
z1 <- c(10,20,30)
z2 <- c(20,30,40)
modality <- "all"
truncate <- TRUE
as.list <- FALSE

# internal method comparisons
.multiglom <- aqp:::.multiglom
.multiglomDT <- aqp:::.multiglomDT

# both are vectorized over p
expect_equal(.multiglom(p, z1[1], z2[1]),
             .multiglomDT(p, z1[1], z2[1]))

# .multiglomDT is vectorized over z1 and z2 as well as p
expect_equal(c(.multiglom(p[1, ], z1[1], z2[1]),
               .multiglom(p[2, ], z1[2], z2[2]),
               .multiglom(p[3, ], z1[3], z2[3])),
             .multiglomDT(p, z1, z2))

# alternating z1/z2
z1 <- c(rep(c(0,55), 5))[1:9]
z2 <- c(rep(c(10,60), 5))[1:9]

data(sp1)
depths(sp1) <- id ~ top + bottom

par(mar=c(1,1,3,1))
plot(sp1, max.depth=60)
plot(glom(sp1, z1, z2, truncate=TRUE), max.depth=60, add=T, color="prop", name = NA)
