


## plot original + diced sketches
.sideBySidePlot <- function(d, s, .color = 'p1', .width = 0.15, .xoffset = -0.45, ...) {
  # original
  plotSPC(d, width = .width, color = .color, name = NA, default.color = 'grey', ...)
  
  # sliced
  plotSPC(s, width = .width + 0.05, color = .color, name = NA, divide.hz = FALSE, default.color = 'grey', x.idx.offset = .xoffset, add = TRUE, cex.id = 0.5, plot.depth.axis = FALSE, show.legend = FALSE)
  
}

set.seed(1010)
d <- lapply(as.character(1:10), random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = FALSE)
d <- do.call('rbind', d)
depths(d) <- id ~ top + bottom

## discreet slices
s <- dice(d)
.sideBySidePlot(d, s, .color = 'p1')

.slices <- c(5)
s <- dice(d, fm = .slices ~ .)
.sideBySidePlot(d, s, .color = 'p1')

.slices <- c(5, 10, 15, 25, 100, 190)
s <- dice(d, fm = .slices ~ .)
.sideBySidePlot(d, s, .color = 'p1')






## introduce errors

# missing depths
d$bottom[2] <- NA

# 0-thickness hz
d$top[20] <- d$bottom[20]

# garbage depths
d$bottom[32] <- 15
d$top[6] <- 95

# missing data
d$p1[23] <- NA

# flag for later
d$zero.thick <- (d$bottom - d$top) == 0
# profiles 1 and 2
profile_id(subset(d, zero.thick))

# verify illogical horizons are gone
x <- HzDepthLogicSubset(d, byhz = TRUE)
# 0-thick hz removed
table(x$zero.thick)

par(mar = c(1, 1, 3, 1))


plotSPC(fillHzGaps(x, to_bottom = NULL), color = '.filledGap')
# ok
all(checkHzDepthLogic(x, fast = TRUE, byhz = TRUE)$valid)


# OK
# 0-depth hz removed with strict = FALSE
s <- dice(d, byhz = FALSE, fill = FALSE, strict = FALSE)
.sideBySidePlot(d, s)


# OK
# note we set `p1` to NA above, this is profile #4 in the collection
s <- dice(d, byhz = FALSE, fill = FALSE)
plotSPC(s, color = 'p1', name = NA, divide.hz = FALSE, default.color = 'grey')
# can't use .sideBySidePlot(d, s) with missing profiles
abline(h = max(d), lwd = 2, lty = 1)
max(d) == max(s)

# OK
# auto-filling of gaps introduced (byhz = TRUE) but no where else
s <- dice(d, byhz = TRUE, fill = FALSE)
.sideBySidePlot(d, s)
abline(h = max(d), lwd = 2, lty = 1)
max(d) == max(s)

# OK
# auto-filling of gaps introduced (byhz = TRUE) and to-depth of SPC
s <- dice(d, byhz = TRUE, fill = TRUE)
.sideBySidePlot(d, s)
abline(h = max(d), lwd = 2, lty = 1)
max(d) == max(s)


# OK
# # auto-filling of gaps introduced (byhz = TRUE) but no where else
s <- dice(d, byhz = TRUE, fill = TRUE, fm = ~ .)
.sideBySidePlot(d, s)
abline(h = max(d), lwd = 2, lty = 1)
max(d) == max(s)

# OK
# auto-filling of gaps introduced (byhz = TRUE) but no where else
# note top = 5, bottom = 6
s <- dice(d, byhz = TRUE, fill = TRUE, fm = 5 ~ .)
.sideBySidePlot(d, s)
abline(h = 5, lwd = 1, lty = 1)

# all profiles should extend to bottom = 133
# auto-filling of gaps introduced (byhz = TRUE) and to max(z)
s <- dice(d, byhz = TRUE, fill = TRUE, fm = 0:132 ~ .)
.sideBySidePlot(d, s)

abline(h = 133, lwd = 1, lty = 1)
all(profileApply(s, max) == 133)




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


###


# quick check
z <- dice(d[1:2, ], pctMissing = TRUE)

par(mar = c(0,1,3,1))
.sideBySidePlot(d[1:2], z, .color = 'hzID')
z$.pctMissing



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
.sideBySidePlot(z, zz, .color = 'p1')

# all hz attr
zz <- dice(z, fm = 1:100 ~ ., byhz = TRUE)
horizonNames(zz)
.sideBySidePlot(z, zz, .color = 'p1')

# single slice
zz <- dice(z, fm = 50 ~ ., byhz = TRUE)
horizonNames(zz)
.sideBySidePlot(z, zz, .color = 'p1')

zz <- dice(z, fm = 5 ~ ., byhz = TRUE)
horizonNames(zz)
.sideBySidePlot(z, zz, .color = 'p1')


# no LHS: all depths
zz <- dice(z, fm =  ~ p1, byhz = TRUE)
horizonNames(zz)
.sideBySidePlot(z, zz, .color = 'p1')


# pctMissing
zz <- dice(z, pctMissing = TRUE, byhz = TRUE)
horizonNames(zz)
.sideBySidePlot(z, zz, .color = 'p1')
.sideBySidePlot(z, zz, .color = '.pctMissing')



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

# gracefully catch corrupt SPC
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







