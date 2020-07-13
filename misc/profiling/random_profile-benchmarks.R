library(aqp)
library(data.table)
library(profvis)


# 10k random profiles

# 10 seconds
system.time(d <- lapply(1:10000, random_profile, n=c(6, 7, 8), n_prop=5, method='LPP', SPC=FALSE))

# 33 seconds
system.time(d <- lapply(1:10000, random_profile, n=c(6, 7, 8), n_prop=5, method='LPP', SPC=TRUE))


pp <- profvis( { d <- lapply(1:10000, random_profile, n=c(6, 7, 8), n_prop=5, method='LPP', SPC=TRUE) })


