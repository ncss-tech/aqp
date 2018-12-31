\name{subsetProfiles-methods}
\docType{methods}
\alias{subsetProfiles}
\alias{subsetProfiles,SoilProfileCollection-method}

\title{Subset SoilProfileCollection Objects.}
\description{This function is used to subset \code{SoilProfileCollection} objects using either site-level or horizon-level attributes, or both.}

\section{Methods}{
\describe{
\item{\code{signature(object = "SoilProfileCollection", s = 'character', h = 'character', ...)}}{}
}}

\value{A \code{SoilProfileCollection} class object.}

\details{The \code{s} argument supplies a fully-quoted search critera for matching via site-level attributes. The \code{h} argument supplies a fully-quoted search criteria for matching via horizon-level attributes. All horizons associated with a single horizon-level match (i.e. out of several, only a single horizon matches the search critera) are returned. See examples for usage.}

\seealso{ \code{\link{profileApply}, \link{site}, \link{horizons}} }

\examples{
# more interesting sample data
data(sp2)
depths(sp2) <- id ~ top + bottom
site(sp2) <- ~ surface

# subset by integer index, note that this does not re-order the profiles
plot(sp2[1:5, ])

# generate an integer index via pattern-matching
idx <- grep('modesto', sp2$surface, ignore.case=TRUE)
plot(sp2[idx, ])

# generate in index via profileApply:
# subset those profiles where: min(ph) < 5.6
idx <- which(profileApply(sp2, function(i) min(i$field_ph, na.rm=TRUE) < 5.6))
plot(sp2[idx, ])
}
\keyword{methods}
\keyword{manip}
