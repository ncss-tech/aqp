\name{texcl_to_ssc}
\alias{texcl_to_ssc}
\alias{ssc_to_texcl}

\title{Convert between sand, silt and clay to texture class and visa versa}
\description{These function converts sand, silt and clay to texture class and visa versa, by mapping to lookup tables.}
\usage{
texcl_to_ssc(x)

ssc_to_texcl(df, rmHzErrors = TRUE, as.is = FALSE, droplevels = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of texture classes than conform to the USDA code conventions (e.g. c, sil, sl, cos)}
  \item{df}{data.frame with columns containing clay|CLAY|sand|SAND in the heading}
  \item{rmHzErrors}{should records that sum(sand + silt + clay) != 0 be removed (default: TRUE)}
  \item{as.is}{logical: should character vectors be converted to factors? (default: TRUE)}
  \item{droplevels}{logical: indicating whether to drop unused levels in factors. This is useful when the results have a large number of unused classes, which can waste space in tables and figures.}
}
  
\details{These function uses a lookup tables in data(soiltexture) to convert sand, silt and clay to texture class and visa versa. If silt is missing it will be calculated as 100 - clay - sand. Also if values are not integers they will be rounded in order to match to the lookup table.

texcl_to_ssc() replicates the functionality described by Levi (2017).
}

\references{
Matthew R. Levi,
\href{https://acsess.onlinelibrary.wiley.com/doi/full/10.2136/sssaj2016.09.0301}{Modified Centroid for Estimating Sand, Silt, and Clay from Soil Texture Class},
Soil Science Society of America Journal,
2017,
81(3):578-588,
ISSN 1435-0661,
https://doi.org/10.2136/sssaj2016.09.0301.
}

\value{A vector with the results.}
\author{Stephen Roecker}
\examples{
\donttest{

texcl <- c("cos", "s", "fs", "vfs", "lcos",
"ls", "lfs", "lvfs", "cosl", "sl", "fsl", 
"vfsl", "l", "sil", "si", "scl", "cl", "sicl", 
"sc", "sic", "c")

test <- texcl_to_ssc(texcl)
head(test <- cbind(texcl, test))


df <- data.frame(clay = 0:100, sand = 100:0)

ssc_to_texcl(df)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{texture}% use one of  RShowDoc("KEYWORDS")
