
## latest tinkering / testing based on updates to fillHzGaps

## TODO: fully integrate new fillHzGaps
##   * always fill / pad?
##   * additional arguments for gaps vs top / bottom?
##   * backwards compatibility with slice


set.seed(1010)
d <- lapply(as.character(1:10), random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = FALSE)
d <- do.call('rbind', d)
depths(d) <- id ~ top + bottom

# introduce horizonation errors
d$bottom[2] <- NA
d$top[20] <- d$bottom[20]
d$bottom[32] <- 15
d$top[6] <- 95
d$p1[23] <- NA

# verify illogical horizons are gone
x <- HzDepthLogicSubset(d, byhz = TRUE)
plotSPC(fillHzGaps(x, to_bottom = NULL), color = '.filledGap')
# ok
all(checkHzDepthLogic(x, fast = TRUE, byhz = TRUE)$valid)


# OK
# note we set `p1` to NA above, this is profile #4 in the collection
s <- dice(d, byhz = FALSE, fill = FALSE)
plotSPC(s, color = 'p1', name = NA, divide.hz = FALSE, default.color = 'grey')
abline(h = max(d), lwd = 2, lty = 1)

# OK
# auto-filling of gaps introduced (byhz = TRUE) but no where else
s <- dice(d, byhz = TRUE, fill = FALSE)
plotSPC(s, color = 'p1', name = NA, divide.hz = FALSE, default.color = 'grey')
abline(h = max(d), lwd = 2, lty = 1)

# OK
# auto-filling of gaps introduced (byhz = TRUE) and to-depth of SPC
s <- dice(d, byhz = TRUE, fill = TRUE)
plotSPC(s, color = 'p1', name = NA, divide.hz = FALSE, default.color = 'grey')
abline(h = max(d), lwd = 2, lty = 1)

# OK
# # auto-filling of gaps introduced (byhz = TRUE) but no where else
s <- dice(d, byhz = TRUE, fill = TRUE, fm = ~ .)
plotSPC(s, color = 'p1', name = NA, divide.hz = FALSE, default.color = 'grey')
abline(h = max(d), lwd = 2, lty = 1)

# OK
# auto-filling of gaps introduced (byhz = TRUE) but no where else
# note top = 5, bottom = 6
s <- dice(d, byhz = TRUE, fill = TRUE, fm = 5 ~ .)
plotSPC(s, color = 'p1', name = NA, divide.hz = FALSE, default.color = 'grey')
abline(h = 5, lwd = 2, lty = 1)

# TODO: BUG
# profiles 10 / 3 / 6 contain data that extends below 132cm -> data extend to 133
# other profiles should extend to 133 as well
# auto-filling of gaps introduced (byhz = TRUE) and to max(z)
s <- dice(d, byhz = TRUE, fill = TRUE, fm = 0:132 ~ .)
plotSPC(s, color = 'p1', name = NA, divide.hz = FALSE, default.color = 'grey')
abline(h = 132, lwd = 2, lty = 1)

max(s[1, ])


### finish updating these


# library(aqp)
library(data.table)

## try problematic OSD
x <- soilDB::fetchOSD('tatum')

# works, O horizon lost
s <- slice(x, fm = 0:100 ~ .)
par(mar = c(0, 0, 0, 0))
plotSPC(s, width = 0.15)

# works
d <- dice(x, fm = 0:100 ~ ., strict = FALSE)
plotSPC(d, width = 0.15)

# works
d <- dice(x, fm = 0:100 ~ ., strict = TRUE, byhz = TRUE)
plotSPC(d, width = 0.15)

# works
d <- dice(x, strict = TRUE, byhz = TRUE)
plotSPC(d, width = 0.15)


## indexes vs. keys
# See the vignette("datatable-secondary-indices-and-auto-indexing") for more details.

# ~ 10 seconds for 10k profiles
# much faster to generate as DF, then promote to SPC at the end
set.seed(1010)
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
plotSPC(z, color = 'hzID', name = NA, divide.hz = FALSE, show.legend = FALSE)
z$.pctMissing


## hmm... sometimes this breaks, strange edge cases

## introduce horizonation errors
z <- d[1:10, ]
z$bottom[2] <- NA
z$top[20] <- z$bottom[20]
z$bottom[32] <- 15
z$top[6] <- 95
z$p1[23] <- NA

# dropping entire profiles, OK
zz <- dice(z, byhz = FALSE, pctMissing = TRUE)

# ok
metadata(zz)$removed.profiles
setdiff(profile_id(z), profile_id(zz))
zz$.pctMissing

# ok
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
plotSPC(zz, color = '.pctMissing', name = NA, divide.hz = FALSE)



## timing / memory use
bench::mark(
  slice_strict = slice(d, fm = 0:100 ~ ., strict = TRUE),
  slice = slice(d, fm = 0:100 ~ ., strict = FALSE),
  dice = dice(d, byhz = FALSE), 
  dice_byhz = dice(d, byhz = TRUE), 
  dice_fm = dice(d, fm = 0:100 ~ .),
  dice_fm_nofill = dice(d, fm = 0:100 ~ ., fill = FALSE),
  dice_no_chk = dice(d, strict = FALSE), 
  dice_no_chk_pctmissing = dice(d, strict = FALSE, pctMissing = TRUE), 
  iterations = 1,
  check = FALSE
)



## compare output dice vs. slice

data(sp4)
depths(sp4) <- id ~ top + bottom

# gap-filling and extension to limits specified in fm are automatically applied (fill = TRUE)
d <- dice(sp4, fm = 0:50 ~ .)
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

## horizons dropped
d <- dice(sp4, fm = 5 ~ ., byhz = TRUE)

## this works, but corrupt profile is dropped
d <- dice(sp4, fm = 5 ~ ., byhz = FALSE)

## stops with suggestion to use `strict = TRUE`
# safely wraps base::seq()
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







