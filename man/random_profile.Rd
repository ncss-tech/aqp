\name{random_profile}
\alias{random_profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Profile}
\description{
Generate a random soil profile according to set criteria, with correlated depth trends.
}
\usage{
random_profile(id, n = c(3, 4, 5, 6), min_thick = 5, max_thick = 30, n_prop = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{a character or numeric id used for this profile}
  \item{n}{vector of possible number of horizons}
  \item{min_thick}{minimum thickness criteria for a simulated horizon}
  \item{max_thick}{maximum thickness criteria for a simulated horizon}
  \item{n_prop}{number of simulated soil properties (columns in the returned dataframe)}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe with the simulated profile.
}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}
\note{See examples for ideas on simulating several profiles at once.}
\seealso{\code{\link{profile_plot}}}
\examples{
# need this
library(soil)

# generate 10 random profiles with default settings:
d.list <- lapply(1:10, function(i) random_profile(i))
d <- do.call('rbind', d.list)

# plot them
par(mfcol=c(1,10))
by(d, d$id, function(i) {
with(i, profile_plot(top, bottom, name, max_depth=max(d$bottom)))
})

# plot profiles using PCA of properties
require(MASS)
require(plotrix)
d.pca <- princomp(~ p1 + p2 + p3, data=d, cor=TRUE)
d$pca1 <- predict(d.pca)[,1]
cols <- color.scale(d$pca1, extremes=c('white','Royalblue'))


par(mfcol=c(1,10))
by(d, d$id, function(i) {
with(i, profile_plot(top, bottom, name, max_depth=max(d$bottom), col=cols, cex.names=0.75))
})


# simple cluster analysis:
p <- profile_compare(d, vars=c('p1','p2','p3'), max_d=50, k=0)
plot(hclust(p))


# stress-test profile comparison functions
d.list <- lapply(1:1000, function(i) random_profile(i))
d <- do.call('rbind', d.list)
rm(d.list)

# 67 seconds on 1.3 Ghz Intel Mac Mini -- D matrix = 192.3 Mb
# more efficient computation, at the expense of precision, with 
# p <- profile_compare(d, vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0, sample_interval=10)
Rprof(memory.profiling=TRUE)
p <- profile_compare(d, vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0)
Rprof(NULL)

# very fast
h <- hclust(p)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
