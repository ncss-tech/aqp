\name{profile_plot.default}
\Rdversion{1.1}
\alias{profile_plot}
\alias{profile_plot.default}
\alias{profile_plot.SoilProfileList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Profile Plot
}
\description{
Generate a simple diagram of a soil profile, with annotated horizon names.
}
\usage{

\method{profile_plot}{SoilProfileList}(d, color = "soil_color", width = 0.2, 
cex.names = 0.5, cex.depth.axis=cex.names, plot.order = 1:d$num_profiles, add = FALSE,
scaling.factor = 1, y.offset = 0, max.depth = d$max_depth, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{a SoilProfileList object}
  \item{color}{the name of the column containing R-compatible color descriptions}
  \item{width}{scaling of profile widths}
  \item{cex.names}{character scaling applied to horizon names}
  \item{cex.depth.axis}{character scaling applied to depth scale}
  \item{plot.order}{a vector describing the order in which individual SoilProfile objects from the parent should be plotted}
  \item{add}{add to an existing figure} 
  \item{scaling.factor}{vertical scaling of the profile heights} 
  \item{y.offset}{vertical offset for top of profiles} 
  \item{max.depth}{lower depth boundary of plot}
  \item{\dots}{other arguments passed into lower level plotting functions}
}
\details{
This function sets the margins before plotting, so previous calls to \code{par(mar=...)} will be overridden.
}
\value{
A new plot of soil profiles is generated, or optionally added to an existing plot.
}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}

\seealso{
\code{\link{initProfileList}}
}
\examples{
library(aqp)
data(sp1)

# add color vector
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# convert to SoilPofileList class object
sp1.list <- initProfileList(sp1)

# plot profiles
profile_plot(sp1.list)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}

