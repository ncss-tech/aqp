\name{ssc_to_texcl}
\alias{ssc_to_texcl}

\title{Convert sand, silt and clay to texture class}
\description{This function converts sand, silt and clay to texture class, by mapping to a lookup table in (/data/soiltexture.rda).}
\usage{
ssc_to_texcl(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{data.frame with columns containing clay|CLAY|sand|SAND in the heading
  }
}
  
\details{This function uses a lookup table to convert sand, silt and clay to texture class. If silt is missing it will be calculated as 100 - clay - sand.
}

\value{A vector with the results.}
\author{Stephen Roecker}
\examples{
\donttest{

df <- data.frame(clay = 0:100, sand = 100:0)

ssc_to_texcl(df)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{texture}% use one of  RShowDoc("KEYWORDS")
