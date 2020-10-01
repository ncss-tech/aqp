library(aqp)
library(soilDB)
library(sharpshootR)
library(latticeExtra)
library(reshape2)

pedons <- fetchNASIS(from='pedons', nullFragsAreZero=TRUE, rmHzErrors = FALSE)


## note: this could include data thrown-out by fetchNASIS defaults  
# special function for pulling RMF child tables
rmf <- get_RMF_from_NASIS_db()
lapply(rmf, head)

sort(table(rmf$RMF$rdxfeatkind))

r <- rmf$RMF
rc <- rmf$RMF_colors

# remove RMF missing a kind
r <- r[which(!is.na(r$rdxfeatkind)), ]

# replace NA percentages with 0
r$rdxfeatpct[is.na(r$rdxfeatpct)] <- 0

# long -> wide format
x <- dcast(r, phiid ~ rdxfeatkind, value.var = 'rdxfeatpct', fun.aggregate = sum)

str(x)
hzidname(pedons)

## this may fail if fetchNASIS exlcudes pedons
## ack!! this clobbers the @horizons slot
horizons(pedons) <- x

plotSPC(pedons[1:20, ], color='masses of oxidized iron')


idx <- which(x$`masses of oxidized iron` > 0)

phrdxfiid.set <- r$phrdxfiid[which(r$phiid %in% x$phiid[idx])]

rc.sub <- rc[which(rc$phrdxfiid %in% phrdxfiid.set), ]

cols <- with(rc.sub, munsell2rgb(colorhue, colorvalue, colorchroma))

previewColors(unique(cols))

cq <- colorQuantiles(cols)
plotColorQuantiles(cq)


idx <- which(rmf$RMF$rdxfeatkind == 'masses of oxidized iron')
ph.idx <- unique(rmf$RMF$phiid[idx])

cols <- rmf$RMF_colors$phrdxfiid %in% rmf$RMF$phrdxfiid[idx]
