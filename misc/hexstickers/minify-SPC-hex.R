# minified SPCs
library(aqp)
library(soilDB)
library(cluster)

# hexSticker package!
# remotes::install_github("GuangchuangYu/hexSticker")

# basal till toposequence -- a classic southern new england catena
f <- fetchOSD(c("Paxton","Montauk","Woodbridge","Ridgebury","Whitman","Catden"))

f <- trunc(f, 0, min(f))
horizons(f) <- horizonColorIndices(f, "hue", "value", "chroma")

d <- NCSP(
  f,
  vars = c("barron_torrent_redness", "buntley_westin"),
  maxDepth = 100,
  k = 0
)

clust <- cluster::diana(d)
dend <- as.dendrogram(as.hclust(clust))
dend.reorder <- reorder(dend, c(1, 5, 8, 7, 6, 2, 4, 3))

png(width = 250, height = 250, filename = "test.png", type = 'cairo', antialias = 'subpixel')
  par(mfrow = c(1, 1), mai = c(0, 0, 0.166, 0))
  plot(
    dend.reorder,
    yaxt = "n",
    leaflab = "none",
    type = "rectangle",
    edgePar = list(col = c(1), lwd = 1, lty = 1)
  )
  par(mar = c(0, 0, 0, 0))
  plotSPC(f, name = NA, divide.hz = FALSE, print.id = FALSE, width = 0.4, y.offset = -5,
          x.idx.offset = 0.25, plot.depth.axis = FALSE, plot.order = order.dendrogram(dend.reorder))
dev.off()

### aqp sticker v1
### modified 2022/09/30 for ragg graphics device compatibility
### unfortunately divide.hz=FALSE doesn't quite work as expected for this device,
### and grid/hexSticker seems to force ragg; TODO: is there a workaround for this?
library(hexSticker)
soil.color1 <- munsell2rgb("7.5YR", 3, 4)
soil.color2 <- munsell2rgb("2.5Y", 9, 2) # OK.. sorta a soil color...
s <- hexSticker::sticker(
  ~{
    par(mfrow = c(2, 1), mai = c(0, 0, 0.5, 0))
    plot(
      dend.reorder,
      yaxt = "n",
      leaflab = "none",
      type = "rectangle",
      edgePar = list(
        col = c(soil.color1),
        lwd = 1,
        lty = 1
      )
    )
    par(mar = c(0, 0, 0, 0), fg = soil.color2)
    plotSPC(
      f,
      name = NA,
      divide.hz = FALSE,
      print.id = FALSE,
      width = 0.4,
      y.offset = -5,
      scaling.factor = 1.5,
      x.idx.offset = 0.25,
      plot.depth.axis = FALSE,
      plot.order = order.dendrogram(dend.reorder)
    )
  },
             package = "aqp", p_size = 26, p_y = 1.5,
             url = "        http://ncss-tech.github.io/aqp/", u_size = 4, u_color = soil.color1,
             h_fill = soil.color2, p_color = soil.color1, h_color = soil.color1,
             s_x = 1, s_y = .85, s_width = 1, s_height = 0.8,
             filename = "misc/sandbox/hexstickers/aqp_sticker_v1.png")
plot(s)

### soilDB sticker v1
###
s <- hexSticker::sticker(
  "misc/hexstickers/static/soildb_flattening.png",
  package = "soilDB",
  p_size = 26,
  p_y = 1.45,
  h_fill = soil.color2,
  p_color = soil.color1,
  h_color = soil.color1,
  s_x = 1,
  s_y = 0.8,
  s_width = 0.5,
  s_height = 0.5,
  filename = "misc/hexstickers/soilDB_sticker_v1.png",
  url = "     http://github.com/ncss-tech/soilDB/", u_size = 4, u_color = soil.color1,
)
plot(s)

### sharpshootR sticker v1
library(soilDB)
library(sp)
s <- sticker(
  "misc/hexstickers/static/sharpshootR_spade.png",
  package = "sharpshootR",
  p_size = 23,
  p_y = 1.4,
  h_fill = soil.color2,
  p_color = soil.color1,
  h_color = soil.color1,
  s_x = 1,
  s_y = 0.7,
  s_width = 0.6,
  s_height = 0.6,
  filename = "misc/hexstickers/sharpshootR_sticker_v1.png",
  url = "        github.com/ncss-tech/sharpshootR/", u_size = 3.8, u_color = soil.color1,
)
plot(s)
