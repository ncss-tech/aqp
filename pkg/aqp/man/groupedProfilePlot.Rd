\name{groupedProfilePlot}
\alias{groupedProfilePlot}

\title{Grouped Soil Profile Plot}
\description{Plot a collection of soil profiles, sorted and labeled by group.}
\usage{
groupedProfilePlot(x, groups, group.name.offset = -5, 
group.name.cex = 0.75, group.line.col = "red", 
group.line.lwd = 2, group.line.lty = 2, ...)
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{groups}{the name of a site-level attribute that defines groups}
  \item{group.name.offset}{vertical offset for group names}
  \item{group.name.cex}{font size for group names}
  \item{group.line.col}{color for line that splits groups}
  \item{group.line.lwd}{width of line that splits groups}
  \item{group.line.lty}{style of line that splits groups}
  \item{\dots}{further arguments to \code{plotSPC}}
}

\author{D.E. Beaudette}

\seealso{\code{\link{plotSPC}}}

\examples{
data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

groupedProfilePlot(sp1, groups='group', max.depth=150, group.name.offset = -10)
}

\keyword{hplots}

