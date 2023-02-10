# minified SPCs
library(aqp)
library(soilDB)
library(cluster)
library(ape)
library(sharpshootR)

# hexSticker package!
# remotes::install_github("GuangchuangYu/hexSticker")

# basal till toposequence -- a classic southern new england catena
f <- fetchOSD(c("Paxton", "Montauk", "Woodbridge", "Ridgebury", "Whitman", "Catden"))

f <- trunc(f, 0, min(f))
horizons(f) <- horizonColorIndices(f, "hue", "value", "chroma")

d <- NCSP(
  f,
  vars = c("barron_torrent_redness", "buntley_westin"),
  maxDepth = 100,
  k = 0, 
  rescaleResult = TRUE
)

clust <- cluster::diana(d)

h <- as.phylo(as.hclust(clust))

h <- ape::rotateConstr(h, c('CATDEN', 'WHITMAN', 'RIDGEBURY', 'MONTAUK', 'WOODBRIDGE', 'PAXTON'))

par(lend = 2, mar = c(0, 0, 0, 0))
plotProfileDendrogram(
  f, 
  clust = as.hclust(h), 
  y.offset = 0.05, 
  scaling.factor = 0.004, 
  width = 0.3, 
  plot.depth.axis = FALSE, 
  name = '', 
  print.id = FALSE, 
  divide.hz = FALSE, 
  dend.width = 5, 
  dend.y.scale = 1.2
)



