
library(bench)
library(data.table)



# ~ 10 seconds for 10k profiles
# much faster to generate as DF, then promote to SPC at the end
d <- lapply(as.character(1:1000), random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = FALSE)

# much faster: rbind + init SPC after making individual profiles
d <- do.call('rbind', d)

depths(d) <- id ~ top + bottom



# mark(check = FALSE, iterations = 1,
#   grow.old = growEmptyHz.old(d, 100),
#   grow.new = growEmptyHz(d, 100)
# )


g <- growEmptyHz(d, 100)

data(sp4)
depths(sp4) <- id ~ top + bottom

par(mar = c(0, 0, 0, 0))

plotSPC(sp4)

g <- growEmptyHz(sp4, 50)
plotSPC(g, color = 'hzID', show.legend = FALSE)
plotSPC(g, color = 'Ca', show.legend = FALSE)

d <- dice(g, fm = 0:50 ~ .)
plotSPC(d, color = 'sliceID', show.legend = FALSE)
plotSPC(d, color = 'Ca', show.legend = FALSE)
