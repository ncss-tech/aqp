\name{profile_compare}
\alias{profile_compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Numerical Soil Profile Comparison}
\description{
Performs a numerical comparison of soil profiles using named properties, based on a weighted, summed, depth-segment-aligned dissimilarity calculation.
}
\usage{
profile_compare(s, vars, max_d, k, sample_interval=NA, 
replace_na = FALSE, add_soil_flag = FALSE, 
return_depth_distances = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{a dataframe with at least 2 columns of soil properties, and an 'id' column for each profile}
  \item{vars}{a vector with named properties that will be used in the comparison}
  \item{max_d}{depth-slices up to this depth are considered in the comparison}
  \item{k}{a depth-weighting coeficient, use '0' for no depth-weighting (see examples below)}
  \item{sample_interval}{use every n-th depth slice instead of every depth slice, useful for working with > 1000 profiles at a time}
  \item{replace_na}{if TRUE, missing data are replaced by maximum dissimilarity (FALSE)}
  \item{add_soil_flag}{(experimental) addition of 'soil'/'non-soil' flag for use when comparing soil profiles with large differences in depth (FALSE)}
  \item{return_depth_distances}{return intermediate, depth-wise dissimilarity results (FALSE)}
}
\details{}
\value{
a dissimilarity matrix object of class 'dist'
}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}
\note{
... based on the work of .... 
}
\seealso{\code{\link{unroll}}, \code{\link{soil.slot}}}
\examples{

## 1. check out the influence depth-weight coef:
require(lattice)
z <- rep(1:100,4)
k <- rep(c(0,0.1,0.05,0.01), each=100)
w <- 100*exp(-k*z)

xyplot(z ~ w, groups=k, ylim=c(105,-5), xlim=c(-5,105), type='l', 
ylab='Depth', xlab='Weighting Factor', 
auto.key=list(columns=4, lines=TRUE, points=FALSE, title="k", cex=0.8, size=3),
panel=function(...) {
	panel.grid(h=-1,v=-1) 
	panel.superpose(...)
	}
)

## 2. basic implementation, requires at least two properties
data(sp1)
d <- profile_compare(sp1, vars=c('prop','group'), max_d=100, k=0.01)

# better plotting with ape package:
require(ape)
require(cluster)
h <- diana(d)
p <- as.phylo(as.hclust(h))
plot(ladderize(p), cex=0.75, label.offset=1, no.margin=TRUE)
tiplabels(col=cutree(h, 3), pch=15)

## 3. other uses of the dissimilarity matrix
require(MASS)
# Sammon Mapping
d.sam <- sammon(d)

# simple plot
dev.off() ; dev.new()
plot(d.sam$points, type = "n", xlim=range(d.sam$points[,1] * 1.5))
text(d.sam$points, labels=row.names(as.data.frame(d.sam$points)), 
cex=0.75, col=cutree(h, 3))


## 4. try out the 'sample_interval' argument
# compute using sucessively larger sampling intervals
data(sp3)
d <- profile_compare(sp3, vars=c('clay','cec','ph'), 
max_d=100, k=0.01)
d.2 <- profile_compare(sp3, vars=c('clay','cec','ph'), 
max_d=100, k=0.01, sample_interval=2)
d.10 <- profile_compare(sp3, vars=c('clay','cec','ph'), 
max_d=100, k=0.01, sample_interval=10)
d.20 <- profile_compare(sp3, vars=c('clay','cec','ph'), 
max_d=100, k=0.01, sample_interval=20)

# check the results via hclust / dendrograms
oldpar <- par(mfcol=c(1,4), mar=c(2,1,2,2))
plot(as.dendrogram(hclust(d)), horiz=TRUE, main='Every Depth Slice')
plot(as.dendrogram(hclust(d.2)), horiz=TRUE, main='Every 2nd Depth Slice')
plot(as.dendrogram(hclust(d.10)), horiz=TRUE, main='Every 10th Depth Slice')
plot(as.dendrogram(hclust(d.20)), horiz=TRUE, main='Every 20th Depth Slice')
par(oldpar)

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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
