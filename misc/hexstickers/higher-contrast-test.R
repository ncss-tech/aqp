## re-create the original hex sticker with more contrast and without manual composition of dend + profiles


library(aqp)
library(soilDB)
library(sharpshootR)
library(cluster)
library(ape)

## probably need the dev version
# remotes::install_github("GuangchuangYu/hexSticker")



# basal till toposequence -- a classic southern New England catena
# s <- c("Paxton", "Montauk", "Woodbridge", "Ridgebury", "Whitman", "Catden")

# profiles that do not need edits
x1 <- fetchOSD(c('WHITMAN', 'RIDGEBURY', 'MONTAUK', 'WOODBRIDGE'))

# profiles that require edits
x2 <- fetchOSD(c('CATDEN', 'PAXTON'))

# no ragged bottom
site(x1)$truncated <- FALSE

# truncate and flag for ragged bottom
x2 <- trunc(x2, 0, 125)
site(x2)$truncated <- TRUE

# combine
x <- combine(x1, x2)


# color indices for clustering
horizons(x) <- horizonColorIndices(x, "hue", "value", "chroma")

d <- NCSP(
  x,
  vars = c("barron_torrent_redness", "buntley_westin"),
  maxDepth = 100,
  k = 0
)

# div. hierarchical clustering
# final order isn't quite right
h <- diana(d)

# rotate dendrogram into original specification
p <- as.phylo(as.hclust(h))
p <- rotateConstr(p, constraint = c('CATDEN', 'WHITMAN', 'RIDGEBURY', 'MONTAUK', 'WOODBRIDGE', 'PAXTON'))


par(mar = c(0, 0, 0, 0), bg = NA, fg = 'black', xpd = NA)
plotProfileDendrogram(
  x, 
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
  dend.width = 0.9,
  # max.depth = 150,
  raggedBottom = 'truncated',
  lwd = 0.25
)




library(hexSticker)

# WTF?
# this can only be run "once" per session, strange grid errors

# left-hand border line segment is missing
# https://github.com/GuangchuangYu/hexSticker/issues/155

s <- sticker(
  ~{
    par(mar = c(0, 0, 0, 0), bg = NA, fg = 'black', xpd = NA)
    plotProfileDendrogram(
      x, 
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
      dend.width = 0.9,
      # max.depth = 150,
      raggedBottom = 'truncated',
      lwd = 0.25
    )
  },
  package = "aqp", 
  h_color = 'black', h_fill = 'white', hsize = 1,
  p_size = 26, p_y = 1.7, p_x = 1, p_color = 'black',
  url = "https://ncss-tech.github.io/aqp/", 
  u_size = 5,
  s_width = 1.85, s_height = 1, s_x = 1.075, s_y = 0.95, 
  filename = "misc/hexstickers/aqp_sticker_v3.png"
)

plot(s)





