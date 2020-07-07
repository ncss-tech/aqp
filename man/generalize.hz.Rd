\name{generalize.hz}
\alias{generalize.hz}

\title{Generalize Horizon Names}
\description{Generalize a vector of horizon names, based on new classes, and REGEX patterns.}
\usage{generalize.hz(x, new, pat, non.matching.code, hzdepm, ...)}

\arguments{
  \item{x}{a character vector of horizon names}
  \item{new}{a character vector of new horizon classes}
  \item{pat}{a character vector of REGEX, same length as \code{x}}
  \item{non.matching.code}{text used to describe any horizon not matching any item in \code{pat}}
  \item{hzdepm}{a numeric vector of horizon mid-points, must not contain NA, same length as \code{x}}
  \item{\dots}{additional arguments passed to \code{grep()} such as \code{perl=TRUE} for advanced REGEX}
}

\value{factor of the same length as \code{x}}


\author{Dylan E. Beaudette}

\examples{
\dontrun{
data(sp1)

# check original distribution of hz designations
table(sp1$name)

# generalize
sp1$genhz <- generalize.hz(sp1$name, 
new=c('O','A','B','C','R'), 
pat=c('O', '^A','^B','C','R'))

# see how we did / what we missed
table(sp1$genhz, sp1$name)


## a more advanced example, requries perl=TRUE
# example data
x <- c('A', 'AC', 'Bt1', '^AC', 'C', 'BC', 'CB')

# new labels
n <- c('A', '^AC', 'C')
# patterns:
# "A anywhere in the name"
# "literal '^A' anywhere in the name"
# "C anywhere in name, but without preceding A"
p <- c('A', '\\^A', '(?<!A)C')

# note additional argument
res <- generalize.hz(x, new = n, pat=p, perl=TRUE)

# double-check: OK
table(res, x)
}
}

\keyword{manip}
