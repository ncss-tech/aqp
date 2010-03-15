\name{prepanel.depth_function}
\Rdversion{1.1}
\alias{prepanel.depth_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pre-Panel Function for panel.depth_function
}
\description{
Compute reasonable xlim values based on upper/lower confidence limits of depth function.
}
\usage{
prepanel.depth_function(x, y, upper, lower, subscripts, groups = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x-value vector passed in from the calling lattice function
}
  \item{y}{
y-value vector passed in from the calling lattice function
}
  \item{upper}{
upper CI vector passed in from the calling lattice function
}
  \item{lower}{
lower CI vector passed in from the calling lattice function
}
  \item{subscripts}{
subscripts vector passed in from the calling lattice function
}
  \item{groups}{
grouping vector passed in from the calling lattice function
}
  \item{\dots}{
other arguments
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
D.E. Beaudette
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{panel.depth_function}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
