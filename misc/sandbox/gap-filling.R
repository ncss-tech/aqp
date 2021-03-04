# library(aqp)

data(sp4)
depths(sp4) <- id ~ top + bottom

# introduce gaps
idx <- c(2, 8, 12)
sp4$top[idx] <- NA

# ok
horizons(sp4)[idx, ]

# remove problematic horizons
x <- HzDepthLogicSubset(sp4, byhz = TRUE)

# gaps and now problematic profiles
par(mar = c(0, 0, 0, 1))
plotSPC(x, width = 0.3, default.color = 'royalblue')

z <- fillHzGaps(x, flag = TRUE)

# BUG: plotSPC can't use logical data for color
z$.filledGap <- as.factor(z$.filledGap)
plotSPC(z, width = 0.3, color = '.filledGap', show.legend = FALSE)


## try again with a lot of bad data
d <- combine(
  lapply(1:100, random_profile, method = 'LPP', SPC = TRUE)
)

# sprinkle NA
d$top[sample(1:nrow(d), size = 50)] <- NA

# remove bad horizons... could lead to SPC corruption
d <- HzDepthLogicSubset(d, byhz = TRUE)

# hard to see
# plotSPC(d, name = NA, color = 'p1', print.id = FALSE)

d.filled <- fillHzGaps(d, flag = TRUE)

table(d.filled$.filledGap)

# hard to see
d.filled$.filledGap <- factor(d.filled$.filledGap)
plotSPC(d.filled, name = NA, color = '.filledGap', print.id = FALSE)

## TODO: growEmptyHz() must fill "up" to 0



