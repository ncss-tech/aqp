\name{initProfile}
\Rdversion{1.1}
\alias{initProfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Initialise a SoilProfile Object}
\description{
Initialize a SoilProfile class object from a dataframe.
}
\usage{
initProfile(d, depth_units = "cm", idcol = "id")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{depth_units}{
%%     ~~Describe \code{depth_units} here~~
}
  \item{idcol}{
%%     ~~Describe \code{idcol} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(d, depth_units='cm', idcol="id")
	{
	
	# probably want to do some kind of checking depth logic here
	
	# possibly some automatic color conversion here 
	
	# using a list to store parts
	d.temp <- list(
	id=unique(as.character(d[, idcol])),
	depth_units=depth_units,
	nhz=nrow(d),
	max_depth=max(c(d$top, d$bottom), na.rm=TRUE),
	data=d
	)
	
	# set class membership and inheritance
	class(d.temp) <- c('SoilProfile','data.frame')
	return(d.temp)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
