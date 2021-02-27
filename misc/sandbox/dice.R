
# library(aqp)
library(data.table)

## try problematic OSD
x <- soilDB::fetchOSD('tatum')

# works, O horizon lost
s <- slice(x, fm = 0:100 ~ .)
par(mar = c(0, 0, 0, 0))
plotSPC(s)

# nope
d <- dice(x, fm = 0:100 ~ ., strict = FALSE)

# works
d <- dice(x, fm = 0:100 ~ ., strict = TRUE, byhz = TRUE)
plotSPC(d)

# works
d <- dice(x, strict = TRUE, byhz = TRUE)
plotSPC(d)


## indexes vs. keys
# See the vignette("datatable-secondary-indices-and-auto-indexing") for more details.

# ~ 10 seconds for 10k profiles
# much faster to generate as DF, then promote to SPC at the end
d <- lapply(as.character(1:10000), random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = FALSE)

# much faster: rbind + init SPC after making individual profiles
d <- do.call('rbind', d)

depths(d) <- id ~ top + bottom

# fake group
site(d)$group <- factor(sample(letters[1:10], size = length(d), replace =TRUE))

plotSPC(d[1:10, ], color = 'p1', show.legend = FALSE)


## DT full outer join ideas
# https://stackoverflow.com/questions/15170741/how-does-one-do-a-full-join-using-data-table
# 

## address TODO and major design questions:
# https://github.com/ncss-tech/aqp/issues/115



# quick check
z <- dice(d[1:2, ], pctMissing = TRUE)

par(mar = c(0,1,3,1))
# suppress hz names
# strange legend, due to character representation of integers
plotSPC(z, color = 'hzID', name = NA, divide.hz = FALSE)

z$.pctMissing



## introduce horizonation errors
z <- d[1:10, ]
z$bottom[2] <- NA
z$top[20] <- z$bottom[20]

z$bottom[32] <- 15
z$bottom[5]
z$top[6] <- 95

z$p1[23] <- NA

# dropping IDs
zz <- dice(z, byhz = FALSE, pctMissing = TRUE)

# ok
metadata(zz)$removed.profiles
setdiff(profile_id(z), profile_id(zz))
zz$.pctMissing

plotSPC(zz, color = 'name', name = NA, divide.hz = FALSE, show.legend = FALSE)
plotSPC(zz, color = 'hzID', name = NA, divide.hz = FALSE, show.legend = FALSE)
plotSPC(zz, color = 'p1', name = NA, divide.hz = FALSE)
plotSPC(zz, color = '.pctMissing', name = NA, divide.hz = FALSE)

# dropping horizons, leaving gaps
zz <- dice(z, byhz = TRUE)

# ok
metadata(zz)$removed.horizons
setdiff(profile_id(z), profile_id(zz))

plotSPC(zz, color = 'hzID', name = NA, divide.hz = FALSE, show.legend = FALSE)
plotSPC(zz, color = 'p1', name = NA, divide.hz = FALSE)


## formula interface
## TODO: this will error when asking for slices that only occur in NA gaps

# select hz attr
zz <- dice(z, fm = 1:100 ~ p1 + p2, byhz = TRUE)
horizonNames(zz)
plotSPC(zz, color = 'p1', name = NA, divide.hz = FALSE)

# all hz attr
zz <- dice(z, fm = 1:100 ~ ., byhz = TRUE)
horizonNames(zz)
plotSPC(zz, color = 'p1', name = NA, divide.hz = FALSE)

# single slice
zz <- dice(z, fm = 50 ~ ., byhz = TRUE)
horizonNames(zz)
plotSPC(zz, color = 'p1', name = NA, divide.hz = FALSE)

# no LHS: all depths
zz <- dice(z, fm =  ~ p1, byhz = TRUE)
horizonNames(zz)
plotSPC(zz, color = 'p1', name = NA, divide.hz = FALSE)


# pctMissing
zz <- dice(z, pctMissing = TRUE, byhz = TRUE)
horizonNames(zz)
plotSPC(zz, color = 'p1', name = NA, divide.hz = FALSE)



## timing / memory use
bench::mark(
  slice_strict = slice(d, fm = 0:100 ~ ., strict = TRUE),
  slice = slice(d, fm = 0:100 ~ ., strict = FALSE),
  dice = dice(d, byhz = FALSE), 
  dice_byhz = dice(d, byhz = TRUE), 
  dice_fm = dice(d, fm = 0:100 ~ .),
  dice_fm_padNA = dice(d, fm = 0:100 ~ ., padNA = TRUE),
  dice_no_chk = dice(d, strict = FALSE), 
  dice_no_chk_pctmissing = dice(d, strict = FALSE, pctMissing = TRUE), 
  iterations = 1,
  check = FALSE
)



## compare output dice vs. slice

data(sp4)
depths(sp4) <- id ~ top + bottom

# enabling NA padding via growEmptyHz()
d <- dice(sp4, fm = 0:50 ~ ., padNA = TRUE)
s <- slice(sp4, fm = 0:50 ~ .)

# slice() always gave one extra slice... dang it
max(d)
max(s)

# combine results
profile_id(d) <- sprintf("%s-dice", profile_id(d))
z <- combine(s, d)

par(mar = c(0, 0, 0, 0))
plotSPC(z, color = 'name', name = NA, width = 0.3, show.legend = FALSE)
plotSPC(z, color = 'Ca', name = NA, width = 0.3, show.legend = FALSE)

## error when asking for slices entirely within gaps

data(sp4)
depths(sp4) <- id ~ top + bottom

sp4$top[1:2] <- NA

## this throws an error
d <- dice(sp4, fm = 5 ~ ., byhz = TRUE)

## this works, but a profile is dropped
d <- dice(sp4, fm = 5 ~ ., byhz = FALSE)

## this breaks at mapply step
d <- dice(sp4, fm = 5 ~ ., strict = FALSE)

# old slice, NA returned
s <- slice(sp4, fm = 5 ~ .)

profile_id(d) <- sprintf("%s-dice", profile_id(d))
z <- combine(s, d)

par(mar = c(0, 0, 0, 0))
plotSPC(z[1:8, ], color = 'Ca')


## develop tests


## key vs. index vs. none


## profile

# get.slice() wastes a lot of time
pp.slice <- profvis(s <- slice(d, 0:100 ~ .))

# most time spent: checHzDepthLogic + mapply
pp.dice <- profvis::profvis(s <- dice(d))







