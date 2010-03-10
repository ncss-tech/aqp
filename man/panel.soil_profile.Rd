\name{panel.soil_profile}
\Rdversion{1.1}
\alias{panel.soil_profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Panel Functions for Plotting Soil Profile Data}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
panel.soil_profile(x, y, groups, subscripts, cols, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{subscripts}{
%%     ~~Describe \code{subscripts} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, y, groups, subscripts, cols, ...)
	{
	
	make.segments <- function(df, lcol, ...)
		{
		
		n_hz <- length(df$prop) / 2
		
		# need at least 2 horizons
		if(n_hz > 1)
			{
			df.new <- data.frame(top=df$bnd[1:n_hz], bottom=df$bnd[(n_hz+1):length(df$prop)], prop=df$prop[1:n_hz])
			
			# print(df.new)
			
			# vertical segments
			panel.segments(df.new$prop, df.new$top, df.new$prop, df.new$bottom, col=lcol)  
			# horizontal segments
			panel.segments(df.new$prop[-n_hz], df.new$bottom[-n_hz], df.new$prop[-1], df.new$top[-1], col=lcol)
			}
			
		else
			{
			print(paste('only 1 horizon, skipping!', df$groups[1]))
			}
		
		}
	
	panel_color <- cols[panel.number()]
	
	# re-make a nice dataframe
	d <- data.frame(prop=x, bnd=y, groups=groups[subscripts])
	
	by(d, d$groups, make.segments, lcol=panel_color, ...)
	
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
