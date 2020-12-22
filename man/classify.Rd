\name{classify}
\alias{classify}
\alias{codify}
\title{
Classification of soil properties
}
\description{
Generic function to classify soil properties using different classification schemes.
}
\usage{
classify(..., system = "salt severity", droplevels = TRUE)
codify(x, droplevels = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{further arguments, usually a named vector of soil properties (e.g. pH = 1:14)}
  \item{system}{character specifying the classification scheme}
  \item{droplevels}{logical indicating whether to drop unused levels in factors. This is useful when the results have a large number of unused classes, which can waste space in tables and figures.}
}
\details{
This function is intended to classify a combination of soil properties according established USDA and FAO schemes, such as salt severity. Often raster data cannot be stored as a character and thus needs to be stored as an integer. Thus, the codify() function is intended to convert integer values back to a factor. }
\value{
a vector of equal length to the inputs of ...}
\references{
Abrol, I., Yadav, J. & Massoud, F. 1988. \href{http://www.fao.org/3/x5871e/x5871e00.htm}{Salt-affected soils and their management}. No. Bulletin 39. Rome, FAO Soils.

FAO. 2006. \href{http://www.fao.org/publications/card/en/c/903943c7-f56a-521a-8d32-459e7e0cdae9/}{Guidelines for soil description}. Rome, Food and Agriculture Organization of the United Nations.

Richards, L.A. 1954. \href{https://www.ars.usda.gov/ARSUserFiles/20360500/hb60_pdf/hb60complete.pdf}{Diagnosis and Improvement of Saline and Alkali Soils}. U. S. Government Printing Office. 166 pp.
}
\author{
Stephen Roecker
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
test <- expand.grid(
  EC  = sort(sapply(c(0, 0.75, 2, 4, 8, 15, 30), function(x) x + c(0, -0.05, 0.05))),
  pH  = c(8.1, 8.2, 8.3, 8.4, 8.5, 8.6),
  ESP = sort(sapply(c(0, 15, 30, 50, 70, 100), function(x) x + c(0, 0.1, -0.1)))
)
test$ss      <- with(test, classify(EC = EC, pH = pH, ESP = ESP, system = "salt severity"))
test$ss_code <- as.integer(test$ss)
test$ss2     <- codify(test$ss_code)
all(test$ss == test$ss2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
