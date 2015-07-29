\name{aggregateColor}
\alias{aggregateColor}

\title{Summarize Soil Colors}
\description{Summarize soil color data, weighted by occurrence and horizon thickness.}
\usage{
aggregateColor(x, groups = "genhz", col = "soil_color")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{groups}{the name of a horizon or site attribute used to group horizons, see examples}
  \item{col}{the name of a horizon-level attribute with soil color specified in hexadecimal (i.e. "#rrggbb")}
}

\value{A list with the following components:

  \item{scaled.data}{a list of colors and associated weights, one item for each generalized horizon label with at least one color specified in the source data}
  \item{aggregate.data}{a data.frame of weighted-mean colors, one row for each generalized horizon label with at least one color specified in the source data}

}

\details{Weights are computed by:
w_i = sqrt(sum(thickness_i)) * n_i
where w_i is the weight associated with color i, thickness_i is the total thickness of all horizons associated with the color i, and n_i is the number of horizons associated with color i. Weights are computed within groups specified by \code{groups}.}

\author{D.E. Beaudette}

\seealso{\code{\link{generalize.hz}}}

\examples{
# load some example data
data(sp1, package='aqp')

# upgrade to SoilProfileCollection and convert Munsell colors
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# generalize horizon names
n <- c('O', 'A', 'B', 'C')
p <- c('O', 'A', 'B', 'C')
sp1$genhz <- generalize.hz(sp1$name, n, p)

# aggregate colors over horizon-level attribute: 'genhz'
a <- aggregateColor(sp1, 'genhz')

# aggregate colors over site-level attribute: 'group'
a <- aggregateColor(sp1, 'group')

# aggregate colors over depth-slices
s <- slice(sp1, c(5, 10, 15, 25, 50, 100, 150) ~ soil_color)
s$slice <- paste0(s$top, ' cm')
a <- aggregateColor(s, 'slice')

\dontrun{
# optionally plot with helper function
if(require(sharpshootR))
  aggregateColorPlot(a)
}

# a more interesting example
\dontrun{
data(loafercreek, package = 'soilDB')

# generalize horizon names using REGEX rules
n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw', 
'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

# remove non-matching generalized horizon names
loafercreek$genhz[loafercreek$genhz == 'not-used'] <- NA
loafercreek$genhz <- factor(loafercreek$genhz)

a <- aggregateColor(loafercreek, 'genhz')

# plot results with helper function
par(mar=c(1,4,4,1))
aggregateColorPlot(a, print.n.hz = TRUE)

# inspect aggregate data
a$aggregate.data
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

