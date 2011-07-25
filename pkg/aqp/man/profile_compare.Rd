\name{profile_compare}
\alias{profile_compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Numerical Soil Profile Comparison}
\description{
Performs a numerical comparison of soil profiles using named properties, based on a weighted, summed, depth-segment-aligned dissimilarity calculation.
}
\usage{
profile_compare(s, vars, max_d, k, sample_interval = NA, 
replace_na = TRUE, add_soil_flag = TRUE, 
return_depth_distances = FALSE, strict_hz_eval = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{a dataframe with at least 2 columns of soil properties, and an 'id' column for each profile. horizon depths must be integers and self-consistent}
  \item{vars}{a vector with named properties that will be used in the comparison}
  \item{max_d}{depth-slices up to this depth are considered in the comparison}
  \item{k}{a depth-weighting coeficient, use '0' for no depth-weighting (see examples below)}
  \item{sample_interval}{use every n-th depth slice instead of every depth slice, useful for working with > 1000 profiles at a time}
  \item{replace_na}{if TRUE, missing data are replaced by maximum dissimilarity (TRUE)}
  \item{add_soil_flag}{The algorithm will generate a 'soil'/'non-soil' matrix for use when comparing soil profiles with large differences in depth (TRUE). This has the side-effect of producing a diagnostic plot illustrating the contents of the soil/non-soil matrix (when there are 100 or fewer profiles in the collection).}
  \item{return_depth_distances}{return intermediate, depth-wise dissimilarity results (FALSE)}
  \item{strict_hz_eval}{should horizons be strictly checked for internal self-consistency? (FALSE)}
}
\details{
Variability in soil depth can interfere significantly with the calculation of between-profile dissimilarity-- what is the numerical ``distance'' (or dissimilarity) between a slice of soil from profile A and the corresponding, but missing, slice from a shallower profile B? Gower's distance metric would yield a NULL distance, despite the fact that intuition suggests otherwise: shallower soils should be more dissimilar from deeper soils. For example, when a 25 cm deep profile is compared with a 50 cm deep profile, numerical distances are only accumulated for the first 25 cm of soil (distances from 26 - 50 cm are NULL). When summed, the total distance between these profiles will generally be less than the distance between two profiles of equal depth. Our algorithm has an option (setting replace_na=TRUE) to replace NULL distances with the maximum distance between any pair of profiles for the current depth slice. In this way, the numerical distance between a slice of soil and a corresponding slice of non-soil reflects the fact that these two materials should be treated very differently (i.e. maximum dissimilarity). 

This alternative calculation of dissimilarities between soil and non-soil slices solves the problem of comparing shallow profiles with deeper profiles. However, it can result in a new problem: distances calculated between two shallow profiles will be erroneously inflated beyond the extent of either profile's depth. Our algorithm has an additional option (setting add_soil_flag=TRUE) that will preserve NULL distances between slices when both slices represent non-soil material. With this option enabled, shallow profiles will only accumulate mutual dissimilarity to the depth of the deeper profile.
	}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
