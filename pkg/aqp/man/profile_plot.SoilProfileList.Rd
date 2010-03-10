\name{profile_plot.SoilProfileList}
\Rdversion{1.1}
\alias{profile_plot.SoilProfileList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a collection of SoilProfile objects.
}
\description{
...
}
\usage{
profile_plot.SoilProfileList(d, color = "soil_color", width = 0.25, cex.names = 0.5, plot_order=1:d$num_profiles, add=FALSE, scaling.factor=1, y.offset=0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{a SoilProfileList object}
  \item{color}{the name of the column containing R-compatible color descriptions}
  \item{width}{scaling of profile widths}
  \item{cex.names}{character scaling applied to horizon names}
  \item{plot.order}{a vector describing the order in which individual SoilProfile objects from the parent should be plotted}
  \item{add}{add to an existing figure} 
  \item{scaling.factor}{vertical scaling of the profile heights} 
  \item{y.offset}{vertical offset for top of profiles} 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{initProfileList}}
}
\examples{
library(aqp)
data(sp1)

# add color vector
missing.colors.idx <- which(is.na(sp1$r))
sp1$soil_color <- NA
sp1$soil_color[-missing.colors.idx] <- with(sp1[-missing.colors.idx,], rgb(r, g, b, max=0.7) )

# convert to SoilPofileList class object
sp1.list <- initProfileList(sp1)

# plot
profile_plot(sp1.list)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
