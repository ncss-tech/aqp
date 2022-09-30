library(aqp)
library(soilDB)
library(lattice)


# fetch KSSL data by series name
sn <- c('musick', 'holland', 'chaix')
g <- fetchKSSL(series = sn, progress = FALSE)

# compute weighted-mean particle diameter for later
g$wmpd <- with(horizons(g), ((vcs * 1.5) + (cs * 0.75) + (ms * 0.375) + (fs * 0.175) + (vfs *0.075) + (silt * 0.026) + (clay * 0.0015)) / (vcs + cs + ms + fs + vfs + silt + clay))

# estimate soil depth based on horizon designations
sdc <- getSoilDepthClass(g, name='hzn_desgn')

# splice-into site data
site(g) <- sdc

# normalize via lower-case
g$taxonname <- tolower(g$taxonname)

# convert taxonname to a factor for grouping
g$taxonname <- factor(g$taxonname)

g.slab <- slab(g, taxonname ~ clay + estimated_ph_h2o + bs82 + wmpd)

saveRDS(g.slab, file = 'misc/slab/slab-1.x-kssl.rds')
