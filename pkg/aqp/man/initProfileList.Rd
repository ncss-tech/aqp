\name{initProfileList}
\Rdversion{1.1}
\alias{initProfileList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Intitalize a SoilProfileList Object}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
initProfileList(d, depth_units = "cm", idcol = "id")
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
	# need to 
	if(!require(plyr))
		stop('Please install the "plyr" package.')
	
	# init list of SoilProfile
	d.list <- list()
	d.list$data <- dlply(.data=d, .variables=idcol, .fun=initProfile, depth_units=depth_units, idcol=idcol)
	
	# add the max depth for the entire list
	d.list$max_depth <- max(sapply(d.list$data, max), na.rm=TRUE)
	
	# add the depth units for the entire list
	d.list$depth_units <- depth_units
	
	# keep count of number of profiles for later
	d.list$num_profiles <- length(d.list$data)
	
	# set class membership
	class(d.list) <- c('SoilProfileList','list')
	
	return(d.list)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
