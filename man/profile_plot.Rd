\name{profile_plot}
\Rdversion{1.1}
\alias{profile_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Profile Plot
}
\description{
Generate a simple diagram of a soil profile, with annotated horizon names.
}
\usage{
profile_plot(top, bottom, name, max_depth, cols = NA, width = 1, cex.names = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{top}{
vector of horizon top boundaries
}
  \item{bottom}{
vector of horizon bottom boundaries
}
  \item{name}{
vector of horizon names
}
  \item{max_depth}{
used for scaling several adjacent plots, in same units as horizon boundaries
}
  \item{cols}{
vector of R-readable colors, used to fill horizons
}
  \item{width}{
relative width of soil profile diagram
}
  \item{cex.names}{
character scaling for horizon names
}
  \item{\dots}{
other arguments to pass on to low-level plotting functions
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot of the soil profile is added to the current graphics device, nothing is returned.
}
\references{
http://casoilresource.lawr.ucdavis.edu/
}
\author{
Dylan E. Beaudette
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link{subplot}}
}
\examples{
library(aqp)

#
# 1. single profile
# 

# fake data
tops <- c(0,3,12,26,60)
bottoms <-c(3,12,26,60,100)
# hz names
hz <- c('Oi','A','Bw','Bt','Cr')

# make plot
profile_plot(top=tops, bottom=bottoms, name=hz)


#
# 2.1 several profiles, with depth scale
#

# use transformed soil colors
data(sp1)
missing.colors.idx <- which(is.na(sp1$r))
sp1$soil_color <- NA
sp1$soil_color[-missing.colors.idx] <- with(sp1[-missing.colors.idx,], rgb(r, g, b, max=0.7) )

setup layout
layout(matrix(c(1,2,3,4,5,6,7,8,9,10),1,10,byrow=FALSE), widths=c(rep(0.1, 9), 0.05))

# add profiles
by(sp1, sp1$id, function(i) {
with(i, profile_plot(top, bottom, name, max_depth=250, col=soil_color, cex.names=1))
})

# add depth scale
par(mar=c(0,0,0,3))
plot(1,1, type='n', xlab='', ylab='', axes=FALSE, ylim=c(250,0))
axis(side=4, line=-1, las=2, at=pretty(c(0,250)), labels=paste(pretty(c(0,250)), 'cm'))


#
# 2.2 several profiles, colored by some property
#
require(plotrix)
data(sp1)

# assign color scale
sp1$color <- color.scale(sp1$prop, c(0,1,1),c(1,1,0),0)

# setup layout
layout(matrix(c(1,10,2,10,3,10,4,10,5,10,6,10,7,10,8,10,9,10),2,9,byrow=FALSE), heights=c(0.9,0.1))

# plot each profile
by(sp1, sp1$id, function(i) {
with(i, profile_plot(top, bottom, name, max_depth=250, col=color, cex.names=0.7))
})

# make an axis along the bottom
vals <- with(sp1, seq(min(prop, na.rm=TRUE), max(prop, na.rm=TRUE), length=10))
par(mar=c(1,4,0,4))
image(x=vals, y=1, z=matrix(vals), col=color.scale(vals, c(0,1,1),c(1,1,0),0), axes=FALSE, ylab='', xlab='')
box(col='grey')
axis(side=3, at=quantile(sp1$prop, na.rm=TRUE), cex.axis=0.9)



#
# 3. add to bottom of dendrogram (requires R >= 2.9)
#
require(plotrix)
require(TeachingDemos)
require(ape)
require(cluster)

data(sp1)
# assign color scale
sp1$color <- color.scale(sp1$prop, extremes=c('white','Royalblue'))

# perform comparison of profiles
d <- profile_compare(sp1, vars=c('prop','group'), max_d=100, k=0.01)
h <- diana(d)
p <- ladderize(as.phylo(as.hclust(h)))

# setup plot
par(mar=c(1,1,1,1))
p.plot <- plot(p, cex=0.5, label.offset=-3, direction='down', y.lim=c(200,0))
tiplabels(col=cutree(h, 3), pch=15)

# get the last plot
lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)

# the order of pedons in dendrogram
d.labels <- attr(d, 'Labels')
new_order <- lastPP$xx[1:lastPP$Ntip]

# add each profile image
# note figure must be large enough to accomodate results...
for(i in 1:lastPP$Ntip)
{
pedon_i <- subset(sp1, subset=id == d.labels[which(new_order == i)])

subplot(with(pedon_i, profile_plot(top, bottom, name, max_depth=250, col=color)), i, 80, size=c(0.75,3.5), type='fig', vadj=1)
}



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
