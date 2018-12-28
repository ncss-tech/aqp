\name{union}
\alias{union}

\title{Combine Multiple SoilProfileCollection Objects}
\description{Safely combine multiple SoilProfileCollection objects that may not share the same internal structure.}

\usage{
union(spc = list(), method = "all")
}

\arguments{
  \item{spc}{a \code{list} of \code{SoilProfileCollection} objects}
  \item{method}{method ("all" or "intersection" [not yet implemented]) used to collect site and horizon level attributes, see details}
}

\details{Method "all" returns all site and horizon level attributes, padded as needed with NA. Method "intersection" (not yet implemented) returns only those site and horizon level attributes that exist in all objects.}

\note{Duplicates are removed.}

\value{a new \code{SoilProfileCollection} object}

\author{D.E. Beaudette and A.G. Brown}


\examples{
# example data
data(sp2, package = 'aqp')
depths(sp2) <- id ~ top + bottom
site(sp2) <- ~ surface

# copy pieces
x <- sp2[1:5, ]
y <- sp2[6:10, ]

# reset IDs and combine
profile_id(y) <- sprintf("%s-copy", profile_id(y))

# this should work
z <- union(list(x,y))

# check
plot(z)
}

\keyword{ manip }% use one of  RShowDoc("KEYWORDS")

