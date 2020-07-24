# minified SPCs
library(aqp)
library(soilDB)
library(cluster)

# hexSticker package!
# remotes::install_github("GuangchuangYu/hexSticker")

# get some different looking soils from around the country
# f <- fetchOSD(c("Agawam","Valentine","Worsham","Cecil","Sites","Scarboro"))

# basal till toposequence -- a classic southern new england catena
f <- fetchOSD(c("Paxton","Montauk","Woodbridge","Ridgebury","Whitman","Catden"))

# generalized sticker, using the 'siblings' OR 'cousins' of a series

my_series <- "Cecil"
sibs <- siblings(my_series, cousins = TRUE)

## SIBLINGS
# 
sibs.maj <- sibs$sib[sibs$sib$majcompflag,]

# ensure order by n in decreasing order, then take top 5
sibs.maj <- sibs.maj[order(sibs.maj$n, decreasing = TRUE)[1:5],]
f <- fetchOSD(c(my_series, sibs.maj$sibling))

# ## COUSINS
# cous.maj <- sibs$cousins[sibs$cousins$majcompflag,]
# cous.maj <- cous.maj[cous.maj$sibling != my_series,]
# 
# # ensure order by n in decreasing order, then 5 at random
# cous.maj.rnd <- cous.maj[order(cous.maj$n, decreasing = TRUE)[sample(1:nrow(cous.maj), 5)],]
# f <- fetchOSD(c(my_series, cous.maj.rnd$sibling))

# we want to use blacklock but it has an oldschool O horizon (described in depths above 0)
# blklock <- fetchOSD('Blacklock')
# blklock@horizons <- blklock@horizons[c(2,1,3:9),]
# delta_cm <- abs(blklock$bottom - blklock$top) * 2.54
#
# # cumulative sum of hz thickness from zero, drop last value for top depth
# blklock@horizons$top <- round(cumsum(c(0, delta_cm[1:nrow(blklock@horizons) - 1])))
#
# # bottom depth is even simpelr, just cumsum
# blklock@horizons$bottom <- round(cumsum(delta_cm))
#
# # combine blacklock in with the others
# f <- aqp::union(list(blklock, f))

f <- trunc(f, 0, min(f))
f <- slice(f, 0:(min(f)-1) ~ soil_color + hue + value + chroma)
horizons(f) <- horizonColorIndices(f, "hue", "value", "chroma")

d <- profile_compare(f,
                     vars=c("barron_torrent_redness", "buntley_westin"),
                     max_d=100,
                     k=0)

clust <- cluster::diana(d)
dend <- as.dendrogram(as.hclust(clust))
dend.reorder <- reorder(dend, c(1, 5, 8, 7, 6, 2, 4, 3))

#png(width=250, height=250, filename="test.png")
  par(mfrow=c(2,1), mai=c(0,0,0.166,0))
  plot(dend.reorder,
       yaxt="n",
       leaflab="none",
       type = "rectangle",
       edgePar = list(col = c(1), lwd = 1, lty=1))
  par(mar=c(0,0,0,0))
  plotSPC(f, name = NA, divide.hz=F, print.id = F, width = 0.4, y.offset=-5,
          x.idx.offset = 0.25, plot.depth.axis = F, plot.order = order.dendrogram(dend.reorder))
#dev.off()

### aqp sticker v1
library(hexSticker)
soil.color1 <- munsell2rgb("7.5YR", 3, 4)
soil.color2 <- munsell2rgb("2.5Y", 9, 2) # OK.. sorta a soil color...
s <- sticker(~{
  par(mfrow=c(2,1), mai=c(0,0,0.5,0))
  plot(dend.reorder,
       yaxt="n",
       leaflab="none",
       type = "rectangle",
       edgePar = list(col = c(soil.color1), lwd = 1, lty=1))
  par(mar=c(0,0,0,0), fg = soil.color2)
  plotSPC(f, name = NA, divide.hz=F, print.id = F, width = 0.4, y.offset=-5, scaling.factor = 1.5,
          x.idx.offset = 0.25, plot.depth.axis = F, plot.order = order.dendrogram(dend.reorder))
},
             package="aqp", p_size=26, p_y = 1.5,
             url = "        http://ncss-tech.github.io/aqp/", u_size = 4, u_color = soil.color1,
             h_fill = soil.color2, p_color = soil.color1, h_color = soil.color1,
             s_x=.8, s_y=.65, s_width=1.4, s_height=1.2,
             filename = "misc/sandbox/hexstickers/aqp_sticker_v1.png")
plot(s)

### soilDB sticker v1
###
s <- sticker(
  "misc/sandbox/hexstickers/static/soildb_flattening.png",
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
  filename = "misc/sandbox/hexstickers/soilDB_sticker_v1.png",
  url = "     http://github.com/ncss-tech/soilDB/", u_size = 4, u_color = soil.color1,
)
plot(s)

### sharpshootR sticker v1
library(soilDB)
library(sp)
s <- sticker(
  "misc/sandbox/hexstickers/static/sharpshootR_spade.png",
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
  filename = "misc/sandbox/hexstickers/sharpshootR_sticker_v1.png",
  url = "        github.com/ncss-tech/sharpshootR/", u_size = 3.8, u_color = soil.color1,
)
plot(s)
