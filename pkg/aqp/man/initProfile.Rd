\name{initProfile}
\Rdversion{1.1}
\alias{initProfile}
\alias{max.SoilProfile}
\alias{[.SoilProfile}

\title{Initialise a SoilProfile Object}
\description{
Initialize a SoilProfile class object from a dataframe.
}
\usage{
initProfile(d, depth_units = "cm", idcol = "id")}

\arguments{
  \item{d}{an input dataframe representing a single soil profile}
  \item{depth_units}{unit of measure for depth values}
  \item{idcol}{a column used to uniquely define this profile from others}
}
\details{This function is probably not all that useful, as most people will be working with several soil profiles at once. It will normally be called iteratvely by \code{\link{initProfileList}}.}
\value{An object of class 'SoilProfile'.}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}

\note{Generic functions that operate on this class are still in development.}

\seealso{
\code{\link{profile_plot}}
}
\examples{
data(sp1)

# subset a single soil profile:
sp1.1 <- subset(sp1, subset=id == 'P001')

# convert into SoilProfile Class object:
sp1.1.sp <- initProfile(sp1.1)

# basic accessor functions (still in development)

# 1. get the depth (bottom of lowest horizon) from this object:
max(sp1.1.sp)

# 2. access the 2nd horizon:
sp1.1.sp[2, ]

# 3. access an attribute
sp1.1.sp[, 'name']


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
