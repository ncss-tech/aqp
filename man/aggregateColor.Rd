% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregateColor.R
\name{aggregateColor}
\alias{aggregateColor}
\title{Summarize Soil Colors}
\usage{
aggregateColor(
  x,
  groups = "genhz",
  col = "soil_color",
  k = NULL,
  profile_wt = NULL,
  mixingMethod = c("estimate", "exact")
)
}
\arguments{
\item{x}{a \code{SoilProfileCollection} object}

\item{groups}{the name of a horizon or site attribute used to group horizons, see examples}

\item{col}{the name of a horizon-level attribute with soil color specified in hexadecimal (i.e. "#rrggbb")}

\item{k}{single integer specifying the number of colors discretized via PAM (\code{\link[cluster:pam]{cluster::pam()}}), see details}

\item{profile_wt}{the name of a site-level attribute used to modify weighting, e.g. area}

\item{mixingMethod}{method used to estimate "aggregate" soil colors, see \code{\link[=mixMunsell]{mixMunsell()}}}
}
\value{
A list with the following components:
\itemize{
\item \code{scaled.data}: a \code{list} of colors and associated weights, one item for each generalized horizon label with at least one color specified in the source data
\item \code{aggregate.data}: a \code{data.frame} of weighted-mean colors, one row for each generalized horizon label with at least one color specified in the source data
}
}
\description{
Summarize soil color data, weighted by occurrence and horizon thickness.
}
\details{
Weights are computed by:
\code{w_i = sqrt(sum(thickness_i)) * n_i}
where \code{w_i} is the weight associated with color \code{i}, \code{thickness_i} is the total thickness of all horizons associated with the color \code{i}, and \code{n_i} is the number of horizons associated with color \code{i}. Weights are computed within groups specified by \code{groups}.

See the \href{http://ncss-tech.github.io/AQP/sharpshootR/aggregate-soil-color.html}{related tutorial for additional examples}.
}
\examples{

# keep examples from using more than 2 cores
data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))

# load some example data
data(sp1, package = 'aqp')

# upgrade to SoilProfileCollection and convert Munsell colors
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# generalize horizon names
n <- c('O', 'A', 'B', 'C')
p <- c('O', 'A', 'B', 'C')
sp1$genhz <- generalize.hz(sp1$name, n, p)

# aggregate colors over horizon-level attribute: 'genhz'
a <- aggregateColor(sp1, groups = 'genhz', col = 'soil_color')

# check results
str(a)

# simple visualization
aggregateColorPlot(a)

}
\seealso{
\code{\link[=generalize.hz]{generalize.hz()}}, \code{\link[=aggregateColorPlot]{aggregateColorPlot()}}
}
\author{
D.E. Beaudette
}
