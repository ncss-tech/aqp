# check results via MDS
library(MASS)
s <- sammon(d)
s.2 <- sammon(d.2)
s.10 <- sammon(d.10)
s.20 <- sammon(d.20)

# check
plot(s$points, axes=FALSE, xlab='', ylab='') ; box()
points(s.2$points, col=2, cex=0.75)
points(s.10$points, col=4, cex=0.5)
points(s.20$points, col=5, cex=0.25)

# convert to mostly consistent scale
sp <- scale(s$points)
sp.2 <- scale(s.2$points)
sp.10 <- scale(s.10$points)
sp.20 <- scale(s.20$points)

# label mid-points
label.pos <- rbind(
(sp.2 + sp) / 2,
(sp.10 + sp.2) / 2,
(sp.20 + sp.10) / 2
)

# check
plot(rbind(sp,sp.2,sp.10,sp.20), type='n', axes=FALSE, xlab='', ylab='') ; box()
points(rbind(sp,sp.2,sp.10,sp.20), pch=15, col=rep(1:4, each=9), cex=0.75)
text(label.pos, label=rep(1:3, each=9), cex=0.75, pos=1)
arrows(sp[,1], sp[,2], sp.2[,1], sp.2[,2], len=0.08, col='grey')
arrows(sp.2[,1], sp.2[,2], sp.10[,1], sp.10[,2], len=0.08, col='grey')
arrows(sp.10[,1], sp.10[,2], sp.20[,1], sp.20[,2], len=0.08, col='grey')
