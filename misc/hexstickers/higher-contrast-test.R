## re-create the original hex sticker with more contrast and without manual composition of dend + profiles


library(aqp)
library(soilDB)
library(sharpshootR)
library(cluster)
library(ape)

# basal till toposequence -- a classic southern New England catena
s <- toupper(c("Paxton", "Montauk", "Woodbridge", "Ridgebury", "Whitman", "Catden"))

f <- fetchOSD(s)

horizons(f) <- horizonColorIndices(f, "hue", "value", "chroma")

d <- NCSP(
  f,
  vars = c("barron_torrent_redness", "buntley_westin"),
  maxDepth = 100,
  k = 0
)

h <- diana(d)

# rotate dendrogram into original specification
p <- as.phylo(as.hclust(h))
p <- rotateConstr(p, constraint = c('CATDEN', 'WHITMAN', 'RIDGEBURY', 'MONTAUK', 'WOODBRIDGE', 'PAXTON'))


par(mar = c(0, 0, 0, 0), bg = 'white', fg = 'black')
plotProfileDendrogram(
  f, 
  clust = as.hclust(p), 
  print.id = FALSE,
  name = NA,
  # name.style = 'center-center',
  scaling.factor = 0.7,
  y.offset = 3,
  depth.axis = FALSE,
  hz.distinctness.offset = 'hzd',
  # divide.hz = FALSE,
  width = 0.3, 
  dend.width = 2.5,
  max.depth = 150,
  lwd = 1
  )


