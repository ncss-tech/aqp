
# library(aqp)
library(data.table)

## indexes vs. keys
# See the vignette("datatable-secondary-indices-and-auto-indexing") for more details.

# ~ 10 seconds for 10k profiles
# much faster to generate as DF, then promote to SPC at the end
d <- lapply(as.character(1:1000), random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = FALSE)

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




z <- aqp:::.dice(d[1:2, ])

par(mar = c(0,1,3,1))
# suppress hz names
# strange legend, due to character representation of integers
plotSPC(z[2, ], color = 'hzID', name = NA, divide.hz = FALSE)



## introduce horizonation errors

z <- d[1:10, ]
z$bottom[2] <- NA
z$top[20] <- z$bottom[20]

z$bottom[32] <- 15
z$bottom[5]
z$top[6] <- 95

# dropping IDs
zz <- aqp:::.dice(z, byhz = FALSE, dropInvalid = TRUE)

# ok
metadata(zz)$dice.removed.profiles
setdiff(profile_id(z), profile_id(zz))

plotSPC(zz, color = 'hzID', name = NA, divide.hz = FALSE)

# dropping horizons (a good idea?)
zz <- aqp:::.dice(z, byhz = TRUE, dropInvalid = TRUE)

# ok
metadata(zz)$dice.removed.horizons
setdiff(profile_id(z), profile_id(zz))

plotSPC(zz, color = 'hzID', name = NA, divide.hz = FALSE)









## TODO: evaluate memory footprint

## current slice()
# 10k profiles: 15 seconds (home MacOS)
# 100k profiles: 266 seconds
system.time(s <- slice(d, 0:100 ~ .))

## 1x mapply, merge.data.table
# 10k profiles: 3 seconds (home MacOS)
# 100k profiles: 42 seconds
system.time(s <- aqp:::.dice(d))


## profile

# get.slice() wastes a lot of time
pp.slice <- profvis(s <- slice(d, 0:100 ~ .))

# most time spent: checHzDepthLogic + mapply
pp.dice <- profvis(s <- aqp:::.dice(d))







