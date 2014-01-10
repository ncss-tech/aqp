\name{addVolumeFraction}
\alias{addVolumeFraction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addVolumeFraction(x, colname, res = 10, cex.min = 0.1, cex.max = 0.5, pch = 15, col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{colname}{
%%     ~~Describe \code{colname} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{cex.min}{
%%     ~~Describe \code{cex.min} here~~
}
  \item{cex.max}{
%%     ~~Describe \code{cex.max} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
# sample data
data(loafercreek, package='soilDB')

# subset first 10 profiles 
s <- loafercreek[1:10, ]

# replace 0% frags with NA
s$total_frags_pct[which(s$total_frags_pct == 0)] <- NA

# plot in random order, note that annotations follow
par(mar=c(0, 0, 3, 0))
plot(s, color='total_frags_pct', plot.order=sample(1:length(s)))
addVolumeFraction(s, 'total_frags_pct', pch=1)

par(mar=c(0, 0, 0, 0))
plot(s, max.depth=100, name='total_frags_pct', cex.name=1, axis.line.offset=-4.25)
addVolumeFraction(s, 'total_frags_pct', pch=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
