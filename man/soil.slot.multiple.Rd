\name{soil.slot.multiple}
\Rdversion{1.1}
\alias{soil.slot.multiple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate Soil Profile Slotting
}
\description{
Align several soil properties to a user-defined basis, with data returned in long format.
}
\usage{
soil.slot.multiple(data, g, vars, seg_size=NA, strict=FALSE, user.fun=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{an input dataframe with 'id' column defining soil profiles, horizon depths must be integers}
  \item{g}{a character string or vector of length = 1, naming the column in the input dataframe to use as the grouping variable}
  \item{vars}{a character vector with names of columns in the input dataframe that will be aggregated}
  \item{seg_size}{User-degined segment size, default is 1}
  \item{strict}{should horizons be strictly checked for self-consistency? defaults to FALSE}
  \item{user.fun}{User-defined function that should accept a vector and return a scalar. This function should know how to properly deal with unexpected, NA, NULL, or Inf values and trap them accordingly.}
}
\details{
This function is a wrapper to \code{\link{soil.slot}}, that makes it simpler to "slice" several soil properties at once. The results are in long format; see examples for further processing steps that may be required.
}
\note{\code{soil.slot.multiple} does not currently support a user-defined slotting vector}
\value{
A dataframe similar to the output from \code{\link{soil.slot}}, but with depth-slice information repeated for each requested input variable.
}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{soil.slot}}, \code{\link{unroll}}
}
\examples{
data(sp3)

# add new grouping factor
sp3$group <- 1
sp3$group[as.numeric(sp3$id) > 5] <- 2
sp3$group <- factor(sp3$group)

# slot several variables at once
# within each level of 'group'
# according to 5 cm vertcal increments
a <- soil.slot.multiple(sp3, g='group', vars=c('L','A', 'B'), seg_size=5)

# pre-compute intervals
a$upper <- with(a, p.mean + p.sd)
a$lower <- with(a, p.mean - p.sd)

# check the results
xyplot(
top ~ p.mean | variable, data=a, groups=groups, subscripts=TRUE, 
lower=a$lower, upper=a$upper, ylim=c(125,-5), alpha=0.5,
layout=c(3,1), scales=list(x=list(relation='free')),
panel=panel.depth_function, 
prepanel=prepanel.depth_function
)

# convert mean value for each variable into long format
a.wide <- cast(a, groups + top + bottom ~ variable, value=c('p.mean'))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
