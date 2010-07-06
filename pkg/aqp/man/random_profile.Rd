\name{random_profile}
\alias{random_profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Profile}
\description{
Generate a random soil profile according to set criteria, with correlated depth trends.
}
\usage{
random_profile(id, n = c(3, 4, 5, 6), min_thick = 5,
 max_thick = 30, n_prop = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{a character or numeric id used for this profile}
  \item{n}{vector of possible number of horizons}
  \item{min_thick}{minimum thickness criteria for a simulated horizon}
  \item{max_thick}{maximum thickness criteria for a simulated horizon}
  \item{n_prop}{number of simulated soil properties (columns in the returned dataframe)}
}

\value{
A dataframe with the simulated profile.
}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}
\note{See examples for ideas on simulating several profiles at once.}
\seealso{\code{\link{profile_compare}}}
\examples{
# need this
library(aqp)

# generate 10 random profiles with default settings:
d.list <- lapply(1:10, function(i) random_profile(i))
d <- do.call('rbind', d.list)

# convert to SoilProfileList and plot
# add a fake color
d$soil_color <- 'white'

# if the plotrix package is installed, 
# make a more interesting color based on the first property
if(require(plotrix))
	d$soil_color <- color.scale(d$p1, c(0,1,1),c(1,1,0),0)

# convert to SoilProfileList object		
d <- initProfileList(d)
profile_plot(d)


# simple cluster analysis:
d.list <- lapply(1:10, function(i) random_profile(i))
d <- do.call('rbind', d.list)
p <- profile_compare(d, vars=c('p1','p2','p3'), max_d=50, k=0)
plot(hclust(p))


# stress-test profile comparison functions (not run)
# d.list <- lapply(1:1000, function(i) random_profile(i))
# d <- do.call('rbind', d.list)
# rm(d.list) ; gc()
# 
# 100 profiles, 4 varibales:
# 67 seconds on 1.3 Ghz Intel Mac Mini 
# D matrix = 192.3 Mb
# p <- profile_compare(d, vars=c('p1','p2','p3','p4','p5'), max_d=50, k=0)

# more efficient computation, at the expense of precision, with 
# p <- profile_compare(d, vars=c('p1','p2','p3','p4','p5'),
# max_d=50, k=0, sample_interval=10)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
