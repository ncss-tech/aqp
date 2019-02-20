library(soilDB)


# single profile, shift 0.1 to the right
x <- fetchOSD('aiken')
par(mar=c(1,0,0,4), fg='white', bg='black', xpd=NA)
plotSPC(x, cex.names=1, axis.line.offset = 0.1, width=0.15, x.idx.offset = 0.1)

# 2 profiles, shift slightly more to the right, increase widths
x <- fetchOSD(c('aiken', 'zook'))
par(mar=c(1,0,0,4), fg='white', bg='black', xpd=NA)
plotSPC(x, cex.names=1, axis.line.offset = 0.1, width=0.25, x.idx.offset = 0.15)

# 3 profiles, increase right margin slightly
x <- fetchOSD(c('aiken', 'zook', 'tristan'))
par(mar=c(1,0,0,4.5), fg='white', bg='black', xpd=NA)
plotSPC(x, cex.names=1, axis.line.offset = 0.1, width=0.25, x.idx.offset = 0.15)

# 4 profiles, no change
x <- fetchOSD(c('aiken', 'zook', 'tristan', 'pierre'))
par(mar=c(1,0,0,4.5), fg='white', bg='black', xpd=NA)
plotSPC(x, cex.names=1, axis.line.offset = 0.1, width=0.25, x.idx.offset = 0.15)
