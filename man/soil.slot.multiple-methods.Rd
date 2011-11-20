\name{soil.slot.multiple-methods}
\docType{methods}
\alias{soil.slot.multiple}
\alias{soil.slot.multiple,data.frame-method}
\alias{soil.slot.multiple,SoilProfileCollection-method}

\title{
Multivariate Soil Profile Slotting
}
\description{
Align several soil properties to a user-defined basis, with data returned in long format.
}


\section{Methods}{
\describe{
\item{data = "SoilProfileCollection"}{ see \link{SoilProfileCollection} }
\item{data = "data.frame"}{ see \link{soil.slot} }
}}

\arguments{
  \item{data}{a dataframe with columns ('id', 'top', 'bottom')}
  \item{fm}{a formula in the form: groups ~ var1 + var2 + var3 + ... OR ~ var1 + var2 + var3 + ...}
  \item{...}{further arguments passed onto \code{\link{soil.slot}}}
}

\details{
This function is a wrapper to \code{\link{soil.slot}}, that makes it simpler to "slice" several soil properties at once. The results are in long format; see examples for further processing steps that may be required.
}
\value{
A dataframe similar to the output from \code{\link{soil.slot}}, but with depth-slice information repeated for each requested input variable and group. If a grouping variable is not given, then all of the profiles in the collection are aggregated into a single representative profile.
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
# using default parameters to soil.slot()
a <- soil.slot.multiple(sp3, fm=group ~ L + A + B)

# pre-compute intervals
a$upper <- with(a, p.mean + p.sd)
a$lower <- with(a, p.mean - p.sd)

# check the results:
# note that 'group' is the column containing group labels
xyplot(
top ~ p.mean | variable, data=a, groups=group, subscripts=TRUE, 
lower=a$lower, upper=a$upper, ylim=c(125,-5), alpha=0.5,
layout=c(3,1), scales=list(x=list(relation='free')),
panel=panel.depth_function, 
prepanel=prepanel.depth_function
)


# convert mean value for each variable into long format
a.wide <- cast(a, group + top + bottom ~ variable, value=c('p.mean'))


## again, this time for a user-defined slab from 40-60 cm
a <- soil.slot.multiple(sp3, fm=group ~ L + A + B, seg_vect=c(40,60))

# now we have weighted average properties (within the defined slab) for each variable, and each group
(a.wide <- cast(a, group + top + bottom ~ variable, value=c('p.mean')))

## this time, compute the weighted mean of selected properties, by profile ID
a <- soil.slot.multiple(sp3, fm=id ~ L + A + B, seg_vect=c(40,60))
(a.wide <- cast(a, id + top + bottom ~ variable, value=c('p.mean')))


## aggregate the entire collection: 
## note the missing left-hand side of the formula
a <- soil.slot.multiple(sp3, fm= ~ L + A + B)



## this will work in the next version of plyr
\dontrun{library(doSMP)

# setup global option
options('AQP_parallel'=TRUE)

# register workers (2 cores)
w <- startWorkers(workerCount=2, FORCE=TRUE)
registerDoSMP(w)

# run in parallel
a <- soil.slot.multiple(sp3, fm=group ~ L + A + B, seg_size=5)

# stop workers (critical!)
stopWorkers(w)
}
}

\keyword{methods}
\keyword{manip}
