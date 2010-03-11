\name{panel.depth_function}
\Rdversion{1.1}
\alias{panel.depth_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, y, upper=NA, lower=NA, subscripts=NULL, groups=NULL, ...) {

# add grid
panel.grid(h=-1, v=-1, lty=3, col=1)

# when the length of 'y' is > 'x', we are plotting a step function
if(length(y) > length(x))
	{
		
	# re-make a nice dataframe
	d <- data.frame(prop=x, bnd=y, upper=upper[subscripts], lower=lower[subscripts], groups=groups[subscripts])
	
	by(d, d$groups, make.segments, ...)	
	
	}

else
	{


# if we have an upper and lower bound defined, plot them
if(!missing(upper) & !missing(lower))
	{
	# working with grouped data and paneled data
	if(!missing(groups) & !missing(subscripts))
		{
		d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts], groups=groups[subscripts])
		# levels in the groups, for color matching
		ll <- levels(d$groups)
		}
	
	# no grouping, add a fake group for compatiblity
	if(missing(groups))
		{
		d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts], groups=factor(1))
		# levels in the groups, for color matching
		ll <- levels(d$groups)
		}
	
	
	# add conf. intervals / aggregation uncertainty
	by(d, d$groups, function(d_i) 
		{
		# cannot have NA in values that define polygon boundaries
		d_i <- subset(d_i, subset=is.na(d_i$upper) == FALSE & is.na(d_i$lower) == FALSE)
		
		# make conf.int polygon
		panel.polygon(x=c(d_i$lower, rev(d_i$upper)), y=c(d_i$top, rev(d_i$top)), col=grey(0.7), border=NA, ...)
		})
	}
# no upper, lower bounds
else
	{
	d <- data.frame(yhat=x, top=y, groups=groups[subscripts])
	# levels in the groups, for color matching
	ll <- levels(d$groups)	
	}


# add main lines
by(d, d$groups, function(d_i) 
	{
	# lookup color
	m <- match(unique(d_i$group), ll)
	# add line
	panel.lines(d_i$yhat, d_i$top, lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m], lty=trellis.par.get('superpose.line')$lty[m])
	})

	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
