\name{texcl_to_ssc}
\alias{texcl_to_ssc}
\alias{ssc_to_texcl}
\alias{texmod_to_fragvoltot}
\alias{gsc_to_taxpartsize}

\title{Textural conversions}
\description{These functions consist of several conversions between sand, silt and clay to texture class and visa versa, textural modifers to rock fragments, and grain size composition to the family particle size class.}
\usage{
texcl_to_ssc(texcl = NULL, clay = NULL)

ssc_to_texcl(sand = NULL, clay = NULL, as.is = FALSE, droplevels = TRUE)

texmod_to_fragvoltot(texmod, lieutex = NULL)

gsc_to_taxpartsize(texcl = NULL, clay = NULL, sand = NULL, fragvoltot = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sand}{vector of sand percentages (default: NULL)}
  \item{clay}{vector of clay percentages (default: NULL)}
  \item{fragvoltot}{vector of rock fragment percentages (default: NULL)}
  \item{texcl}{vector of texture classes than conform to the USDA code conventions (e.g. c|C, sil|SIL, sl|SL, cos|COS)}
  \item{texmod}{vector of textural modifers that conform to the USDA code convenctions (e.g. gr|GR, grv|GRV)}
  \item{lieutex}{vector of in lieu of texture terms that conform to the USDA code convenctions (e.g. gr|GR, pg|PG), only used when fragments or artifacts are > 90 percent by volume (default: NULL))}
  \item{as.is}{logical: should character vectors be converted to factors? (default: TRUE)}
  \item{droplevels}{logical: indicating whether to drop unused levels in factors. This is useful when the results have a large number of unused classes, which can waste space in tables and figures.}
}
  
\details{These functions are intended to impute missing values or check existing values. The ssc_to_texcl() function uses the same logic as the particle size estimator calculation in NASIS to classify sand and clay into texture class. The results are stored in data(soiltexture) and used by texcl_to_ssc() as a lookup table to convert texture class to sand, silt and clay. The function texcl_to_ssc() replicates the functionality described by Levi (2017).
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

# example ssc_to_texcl()
tex <- expand.grid(sand = 0:100, clay = 0:100)
tex <- subset(tex, (sand + clay) < 101)
tex$texcl <- ssc_to_texcl(sand = tex$sand, clay = tex$clay)
head(tex)


# example texcl_to_ssc(texcl)
texcl <- c("cos", "s", "fs", "vfs", "lcos", "ls", 
"lfs", "lvfs", "cosl", "sl", "fsl", "vfsl", "l", 
"sil", "si", "scl", "cl", "sicl", "sc", "sic", "c"
)
test <- texcl_to_ssc(texcl)
head(test <- cbind(texcl, test), 10)


# example texcl_to_ssc(texcl, clay)
data(soiltexture)
st <- soiltexture$values
idx <- sample(1:length(st$texcl), 10)
ssc <- texcl_to_ssc(texcl = st$texcl)
head(cbind(texcl = st[idx, "texcl"], ssc[idx, ]))


# example texmod_to_fragvoltol
frags <- c("gr", "grv", "grx", "pgr", "pgrv", "pgrx")
head(texmod_to_fragvoltot(frags))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{texture}% use one of  RShowDoc("KEYWORDS")
