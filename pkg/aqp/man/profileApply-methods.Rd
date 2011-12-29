\name{profileApply-methods}
\docType{methods}
\alias{profileApply}
\alias{profileApply,SoilProfileCollection-method}
\title{Apply a function to soil profiles within a SoilProfileCollection object.}
\description{Apply a function to soil profiles within a SoilProfileCollection object.}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{}
}}

\value{A vector of length \code{nrow(horizons(object))} (horizon data) or of length \code{length(object)} (site data).}

\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom

# rescale properties within each profile
sp1$d <- profileApply(sp1, FUN=function(x) round(scale(x$prop), 2))
plot(sp1, name='d')

# compute depth-wise differencing by profile
f <- function(x) { c(x$prop[1], diff(x$prop)) }
sp1$d <- profileApply(sp1, FUN=f)
plot(sp1, name='d')


# compute profile-means, and save to @site
# there must be some data in @site for this to work
site(sp1) <- ~ group
sp1$mean_prop <- profileApply(sp1, FUN=function(x) mean(x$prop, na.rm=TRUE))

# re-plot using ranks defined by computed summaries (in @site)
plot(sp1, plot.order=rank(sp1$mean_prop))

}
\keyword{methods}
\keyword{manip}
