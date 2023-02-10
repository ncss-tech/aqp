# minified SPCs
library(aqp)
library(soilDB)
library(cluster)
library(ape)
library(sharpshootR)

# hexSticker package!
# remotes::install_github("GuangchuangYu/hexSticker")

library(hexSticker)

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

soil.color1 <- munsell2rgb("7.5YR", 3, 4)
soil.color2 <- munsell2rgb("2.5Y", 9, 2) # OK.. sorta a soil color...

par(lend = 2, mar = c(0, 0, 0.5, 0), fg = soil.color2)

plotProfileDendrogram(
  f, 
  clust = as.hclust(h), 
  y.offset = 0.07, 
  scaling.factor = 0.0053, 
  width = 0.4, 
  plot.depth.axis = FALSE, 
  name = '', 
  print.id = FALSE, 
  divide.hz = FALSE, 
  lwd = 0.1,
  dend.width = 5,
  dend.color = soil.color1,
  dend.y.scale = 1.35
)



## not quite right, grid / ragg device ..?

s <- hexSticker::sticker(
  ~{
    par(lend = 2, mar = c(0, 0, 0.5, 0), fg = soil.color2)
    
    plotProfileDendrogram(
      f, 
      clust = as.hclust(h), 
      y.offset = 0.07, 
      scaling.factor = 0.0053, 
      width = 0.4, 
      plot.depth.axis = FALSE, 
      name = '', 
      print.id = FALSE, 
      divide.hz = FALSE,
      lwd = 0.1,
      dend.width = 1,
      dend.color = soil.color1,
      dend.y.scale = 1.35
    )
    
    
  },
  package = "aqp", p_size = 26, p_y = 1.5,
  url = "        http://ncss-tech.github.io/aqp/", u_size = 4, u_color = soil.color1,
  h_fill = soil.color2, p_color = soil.color1, h_color = soil.color1,
  s_x = 1, s_y = .85, s_width = 1, s_height = 0.8,
  filename = "testing.svg")

plot(s)



