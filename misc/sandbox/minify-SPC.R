# minified SPCs
library(aqp)
library(soilDB)
library(cluster)

# get some different looking soils from around the country
f <- fetchOSD(c("Agawam","Houston Black","Worsham","Musick",
                "Scituate","Valentine","Pierre","Cecil"))

# we want to use blacklock but it has an oldschool O horizon (described in depths above 0)
blklock <- fetchOSD('Blacklock')
blklock@horizons <- blklock@horizons[c(2,1,3:9),]
delta_cm <- abs(blklock$bottom - blklock$top) * 2.54

# cumulative sum of hz thickness from zero, drop last value for top depth
blklock@horizons$top <- round(cumsum(c(0, delta_cm[1:nrow(blklock@horizons) - 1])))

# bottom depth is even simpelr, just cumsum 
blklock@horizons$bottom <- round(cumsum(delta_cm))

# combine blacklock in with the others
f <- aqp::union(list(blklock, f))

f <- glomApply(f, function(p) c(0,99), truncate = TRUE)
f <- slice(f, 0:98 ~ soil_color + hue + value + chroma)
horizons(f) <- horizonColorIndices(f, "hue", "value", "chroma")

d <- profile_compare(f, 
                     vars=c("barron_torrent_redness", "buntley_westin"), 
                     max_d=100, 
                     k=0)

clust <- cluster::diana(d)
dend <- as.dendrogram(as.hclust(clust))
dend.reorder <- reorder(dend, c(1, 5, 8, 7, 6, 2, 4, 3))

png(width=250, height=250, filename="test.png")
  par(mfrow=c(2,1), mai=c(0,0,0.166,0))
  plot(dend.reorder, 
       yaxt="n", 
       leaflab="none", 
       type = "rectangle",
       edgePar = list(col = c(1), lwd = 1, lty=c(1,3)))
  par(mar=c(0,0,0,0))
  plotSPC(f, name = NA, divide.hz=F, print.id = F, width = 0.4, y.offset=-5,
          x.idx.offset = 0.25, plot.depth.axis = F, plot.order = order.dendrogram(dend.reorder))
dev.off()  

