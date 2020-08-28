# load libraries
library(aqp)
library(soilDB)
library(lattice)
require(agricolae)

# define plotting style
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

# fetch KSSL data by fuzzy-matching of series name
musick <- fetchKSSL('musick')
holland <- fetchKSSL('holland')

# normalize taxonname1
musick$taxonname <- 'musick'
holland$taxonname <- 'holland'

# stack
g <- aqp::union(list(musick, holland))

# compute weighted-mean particle diameter
g$wmpd <- with(horizons(g), ((vcs * 1.5) + (cs * 0.75) + (ms * 0.375) + (fs * 0.175) + (vfs *0.075) + (silt * 0.026) + (clay * 0.0015)) / (vcs + cs + ms + fs + vfs + silt + clay))

# tabulate number of pedons by normalized series name
table(g$taxonname)

# plot the grouped object, with profiles arranged by taxonname
par(mar=c(0,1,5,1))
groupedProfilePlot(g, groups = 'taxonname', color='sand', group.name.offset = -10, print.id=FALSE)
groupedProfilePlot(g, groups = 'taxonname', color='wmpd', group.name.offset = -10, print.id=FALSE)
groupedProfilePlot(g, groups = 'taxonname', color='estimated_ph_h2o', group.name.offset = -10, print.id=FALSE)

# # 3214: composite horizons ---> overlapping depths
# g[profile_id(g) == 3214, ]
# 
# s[profile_id(s) == 3214, 20]



# hmm errors with default strict=TRUE
s <- slice(g, 0:100 ~ sand + wmpd + estimated_ph_h2o, strict = FALSE)


HSD <- lapply(1:100, function(i) {
  ## TODO: must have error-trapping
  mod <- aov(s[, i]$estimated_ph_h2o ~ s[, i]$taxonname)
  res <- TukeyHSD(mod)
  
  # results: only works with A/B style testing of two groups
  d <- data.frame(
    top = i,
    bottom = i + 1,
    as.data.frame(res[[1]])
  )
  
  return(d)
})

HSD <- do.call('rbind', HSD)

head(HSD)
hist(HSD$p.adj)

  


