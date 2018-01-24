\name{addBracket}
\alias{addBracket}
\alias{addDiagnosticBracket}
\title{Add Depth Brackets}
\description{Add depth-wise brackets to an existing plot of a SoilProfileCollection object.}
\usage{
addBracket(x, label.cex=0.75,
tick.length = 0.05, arrow.length = 0.05, offset = -0.3, 
missing.bottom.depth = 25, 
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data.frame containing ID, `top`, `bottom`, and optionally `label` columns}
  \item{label.cex}{scaling factor for label font}
  \item{tick.length}{length of bracket "tick" mark}
  \item{arrow.length}{length of arrowhead}
  \item{offset}{numeric, length of left-hand offset from each profile}
  \item{missing.bottom.depth}{distance (in depth units) to extend brackets that are missing a lower depth}
  \item{\dots}{further arguments passed on to \code{segments} or \code{arrows}}
}
\details{...TODO...}

\note{This is a `low-level` plotting function: you must first plot a \code{SoilProfileCollection} object before using this function.}

\author{D.E. Beaudette}

\seealso{\code{\link{plotSPC}}}
\examples{
library(plyr)

# sample data
data(sp1)

# add color vector
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom

# plot profiles
plot(sp1)

# extract top/bottom depths associated with all A horizons
# return as a single data.frame / profile
f <- function(i) {
  h <- horizons(i)
  idx <- grep('^A', h$name)
  res <- data.frame(top=min(h$top[idx]), bottom=max(h$bottom[idx], na.rm=TRUE))
  return(res)
}

# apply function to each profile in sp1, result is a list
a <- profileApply(sp1, f, simplify=FALSE)
# convert list into data.frame
a <- ldply(a)
# set idname so that addBrackets can locate the correct profile
names(a)[1] <- idname(sp1)

# plot
plot(sp1)
# annotate with brackets
# note that plotting order is derived from the call to `plot(sp1)`
addBracket(a, col='red')

# brackets follow plotting order
plot(sp1, plot.order=length(sp1):1)
# annotate with brackets
# note that plotting order is derived from the call to `plot(sp1)`
addBracket(a, col='red')


# more interesting example using diagnostic horizons
if(require(soilDB)) {
  # load some sample data with diagnostic horizons
  data("gopheridge")
  
  # extract first 15 profiles
  x <- gopheridge[1:15, ]
  
  # plot
  par(mar=c(0,0,0,0))
  plot(x, name='hzname')
  
  # add brackets describing the argillic horizon
  addDiagnosticBracket(x, kind='argillic horizon', col='red')
  # add brackets describing paralithic contact
  addDiagnosticBracket(x, kind='paralithic contact', col='blue')
  
  # adjust plot order, brackets will follow:
  par(mar=c(0,0,0,0))
  plot(x, name='hzname', plot.order=15:1)
  
  # add brackets describing the argillic horizon
  addDiagnosticBracket(x, kind='argillic horizon', col='red')
  # add brackets describing paralithic contact
  addDiagnosticBracket(x, kind='paralithic contact', col='blue')
}


}
