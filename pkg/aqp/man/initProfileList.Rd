\name{initProfileList}
\Rdversion{1.1}
\alias{initProfileList}
\alias{[.SoilProfileList}

\title{Intitalize a SoilProfileList Object}
\description{
Initialize a SoilProfileList class object from a dataframe.
}
\usage{
initProfileList(d, depth_units = "cm", idcol = "id")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{an input dataframe representing a single soil profile}
  \item{depth_units}{unit of measure for depth values}
  \item{idcol}{a column used to uniquely define this profile from others}
}
\details{This function will generate a SoilProfileList object by passing each soil profile in the input dataframe to \code{link{initProfile}}.}

\value{An object of class 'SoilProfileList'.}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}

\seealso{\code{\link{initProfile}}}

\examples{
data(sp1)

# convert into SoilProfileList object
sp1.splist <- initProfileList(sp1)

# basic accessor functions (still in development)

# 1. get the first horizon from each soil profile 
sp1.splist[1,]

# 2 get the first profile as a SoilProfile object
sp1.splist[, 1]

# 3. get a named column from the second profile
sp1.splist[, 2][, 'name']

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
