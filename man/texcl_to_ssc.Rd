\name{texcl_to_ssc}
\alias{texcl_to_ssc}
\alias{ssc_to_texcl}
\alias{texmod_to_fragvoltot}
\alias{texture_to_taxpartsize}

\title{Textural conversions}
\description{These functions consist of several conversions between sand, silt and clay to texture class and visa versa, textural modifers to rock fragments, and grain size composition to the family particle size class.}

\usage{
texcl_to_ssc(texcl = NULL, clay = NULL, sample = FALSE)

ssc_to_texcl(sand = NULL, clay = NULL, as.is = FALSE, droplevels = TRUE)

texmod_to_fragvoltot(texmod, lieutex = NULL)

texture_to_taxpartsize(texcl = NULL, clay = NULL, sand = NULL, fragvoltot = NULL)
}

\arguments{
  \item{sand}{vector of sand percentages}
  \item{clay}{vector of clay percentages}
  \item{fragvoltot}{vector of rock fragment percentages}
  \item{texcl}{vector of texture classes than conform to the USDA code conventions (e.g. c|C, sil|SIL, sl|SL, cos|COS)}
  \item{texmod}{vector of textural modifers that conform to the USDA code convenctions (e.g. gr|GR, grv|GRV)}
  \item{lieutex}{vector of in lieu of texture terms that conform to the USDA code convenctions (e.g. gr|GR, pg|PG), only used when fragments or artifacts are > 90 percent by volume (default: NULL))}
  \item{as.is}{logical: should character vectors be converted to factors? (default: TRUE)}
  \item{droplevels}{logical: indicating whether to drop unused levels in factors. This is useful when the results have a large number of unused classes, which can waste space in tables and figures.}
  \item{sample}{logical: should ssc be random sampled from the lookup table? (default: FALSE)}
}

\details{These functions are intended to estimate missing values or check existing values. The \code{ssc_to_texcl()} function uses the same logic as the particle size estimator calculation in NASIS to classify sand and clay into texture class. The results are stored in \code{soiltexture} and used by \code{texcl_to_ssc()} as a lookup table to convert texture class to sand, silt and clay. The function \code{texcl_to_ssc()} replicates the functionality described by Levi (2017).

When \code{sample = TRUE}, the results can be used to estimate within-class, marginal distributions of sand, silt, and clay fractions. It is recommended that at least 10 samples be drawn for reasonable estimates.

Unlike the other functions, \code{texture_to_taxpartsize()} is intended to be computed on weighted averages within the family particle size control section. Also recall from the criteria that carbonate clay should be subtracted from clay content and added to silt content. Similarily, if the percent of very fine sand is known it should be substracted from the sand, and added to the silt content.
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

# example of ssc_to_texcl()
tex <- expand.grid(sand = 0:100, clay = 0:100)
tex <- subset(tex, (sand + clay) < 101)
tex$texcl <- ssc_to_texcl(sand = tex$sand, clay = tex$clay)
head(tex)


# example of texcl_to_ssc(texcl)
texcl <- c("cos", "s", "fs", "vfs", "lcos", "ls",
"lfs", "lvfs", "cosl", "sl", "fsl", "vfsl", "l",
"sil", "si", "scl", "cl", "sicl", "sc", "sic", "c"
)
test <- texcl_to_ssc(texcl)
head(test <- cbind(texcl, test), 10)


# example of texcl_to_ssc(texcl, clay)
data(soiltexture)
st <- soiltexture$values
idx <- sample(1:length(st$texcl), 10)
st <- st[idx, ]
ssc <- texcl_to_ssc(texcl = st$texcl)
head(cbind(texcl = st$texcl, clay = ssc$clay))


# example of texmod_to_fragvoltol()
frags <- c("gr", "grv", "grx", "pgr", "pgrv", "pgrx")
head(texmod_to_fragvoltot(frags))


# example of texture_to_taxpartsize()
tex <- data.frame(texcl = c("c", "cl", "l", "ls", "s"),
                  clay  = c(55, 33, 18, 6, 3),
                  sand  = c(20, 33, 42, 82, 93),
                  fragvoltot = c(35, 15, 34, 60, 91))
tex$fpsc <- texture_to_taxpartsize(texcl = tex$texcl,
                                   clay = tex$clay,
                                   sand = tex$sand,
                                   fragvoltot = tex$fragvoltot)
head(tex)


# example of texture_to_taxpartsize() with carbonate clay and very fine sand
carbclay <- rnorm(5, 2, 3)
vfs <- rnorm(5, 10, 3)
st$fpsc <- texture_to_taxpartsize(texcl = tex$texcl,
                                  clay = tex$clay - carbclay,
                                  sand = tex$sand - vfs,
                                  fragvoltot = tex$fragvoltot)
head(tex)


# example of sample = TRUE
texcl <- rep(c("cl", "sil", "sl"), 10)
ssc1 <- cbind(texcl, texcl_to_ssc(texcl = texcl, sample = FALSE))
ssc2 <- cbind(texcl, texcl_to_ssc(texcl = texcl, sample = TRUE))
ssc1$sample <- FALSE
ssc2$sample <- TRUE
ssc  <- rbind(ssc1, ssc2) 
aggregate(clay ~ sample + texcl, data = ssc, summary)
}
}

\keyword{manip}
