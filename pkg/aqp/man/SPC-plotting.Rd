\name{SoilProfileCollection-plotting-methods}
\docType{methods}
\alias{plotSPC}
\alias{plot,SoilProfileCollection-method}


\title{
Profile Plot
}
\description{
Generate a simple diagram of a soil profile, with annotated horizon names.
}

\section{Methods}{
\describe{

\item{\code{signature(x = "SoilProfileCollection")}}{

}
}}

\usage{
plotSPC(x, color='soil_color', width=0.2, name='name', 
cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), print.id=TRUE, 
id.style='side', plot.order=1:length(x), add=FALSE, scaling.factor=1, y.offset=0, 
max.depth=max(x), n.depth.ticks=5, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, 
abbr.cutoff=5, divide.hz=TRUE, \dots)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a SoilProfileCollection object}
  \item{color}{the name of the column containing R-compatible color descriptions}
  \item{width}{scaling of profile widths}
  \item{name}{the name of the column containing the horizon designation}
  \item{cex.names}{character scaling applied to horizon names}
  \item{cex.depth.axis}{character scaling applied to depth scale}
  \item{cex.id}{character scaling applied to profile id}
  \item{print.id}{should the profile id be printed above each profile? (TRUE)}
  \item{id.style}{profile ID printing style: 'top' = centered above each profile, 'side' = 'along the top-left edge of profiles' (default)}
  \item{plot.order}{a vector describing the order in which individual SoilProfile objects from the parent should be plotted}
  \item{add}{add to an existing figure} 
  \item{scaling.factor}{vertical scaling of the profile heights} 
  \item{y.offset}{vertical offset for top of profiles} 
  \item{max.depth}{suggested lower depth boundary of plot}
  \item{n.depth.ticks}{suggested number of ticks in depth scale}
  \item{shrink}{should long horizon names be shrunk by 80\% ?}
  \item{shrink.cutoff}{character length defining long horizon names}
  \item{abbr}{should the profile ID be abbreviated?}
  \item{abbr.cutoff}{suggested minimum length for abbreviated IDs}
  \item{divide.hz}{should horizons be divided with a thin black line? (default is TRUE)}
  \item{\dots}{other arguments passed into lower level plotting functions}
}
\details{
Depth limits (max.depth) and number of depth ticks (n.depth.ticks) are *suggestions* to the \code{pretty()} function. You may have to tinker with both parameters to get what you want. The 'side' \code{id.style} is useful when plotting a large collection of profiles, and/or, when profile IDs are long.
}
\value{
A new plot of soil profiles is generated, or optionally added to an existing plot.
}


\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}

\seealso{
\code{\link{SoilProfileCollection-class}, \link{pretty}}
}
\examples{
data(sp1)

# add color vector
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom

# plot profiles
plot(sp1, id.style='side')

# title, note line argument:
title('Sample Data 1', line=-1, cex.main=0.75)

# plot profiles without horizon-line divisions
plot(sp1, divide.hz=FALSE)

# plot profiles, using alternate profile ID label style
plot(sp1, id.style='side')

# plot horizon color according to some property
# RColorBrewer helps with nice colors
if(require(RColorBrewer)) {
  data(sp3)
  
  # setup colors
  cols <- rev(brewer.pal(8, 'Spectral'))
  cr <- colorRamp(cols)
  
  # assign color based on clay content, rescaled to {0,1}
  sp3$soil_color <- rgb(cr(rescaler(sp3$clay, type='range')), max=255)
  depths(sp3) <- id ~ top + bottom
  plot(sp3)
}



}

