\name{make.segments}
\Rdversion{1.1}
\alias{make.segments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.segments(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
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
function(df)
	{
	
	n_hz <- length(df$prop) / 2
	
	# lookup color
	ll <- levels(df$groups)	
	m <- match(unique(df$group), ll)
	
	# need at least 2 horizons
	if(n_hz > 1)
		{
		# re-make dataframe for plotting segments
		df.new <- data.frame(top=df$bnd[1:n_hz], bottom=df$bnd[(n_hz+1):length(df$prop)], prop=df$prop[1:n_hz])
		
		# vertical segments
		panel.segments(df.new$prop, df.new$top, df.new$prop, df.new$bottom, 
		lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m], 
		lty=trellis.par.get('superpose.line')$lty[m]) 
		 
		# horizontal segments
		panel.segments(df.new$prop[-n_hz], df.new$bottom[-n_hz], df.new$prop[-1], df.new$top[-1], 
		lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m], 
		lty=trellis.par.get('superpose.line')$lty[m])
		}
		
	else
		{
		print(paste('only 1 horizon, skipping!', df$groups[1]))
		}
	
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
