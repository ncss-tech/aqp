\name{panel.depth_function}
\Rdversion{1.1}
\alias{panel.depth_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lattice Panel Function for Soil Profiles
}
\description{
Panel function for plotting grouped soil property data, along with upper and lower estimates of uncertainty.
}
\usage{
panel.depth_function(x, y, upper = NA, lower = NA, subscripts = NULL, groups = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
}
  \item{lower}{
%%     ~~Describe \code{lower} here~~
}
  \item{subscripts}{
%%     ~~Describe \code{subscripts} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(sp1)

xyplot(cbind(top,bottom) ~ prop, groups=id, data=sp1, 
panel=panel.depth_function, ylim=c(250,-10), 
scales=list(y=list(tick.number=10)), xlab='Property', 
ylab='Depth (cm)', main='Soil Profile Averaging by Slotting',
auto.key=list(columns=3, points=FALSE, lines=TRUE)
)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
