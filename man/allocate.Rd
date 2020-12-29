\name{allocate}
\alias{allocate}
\alias{codify}
\title{
Allocate soil properties 
}
\description{
Generic function to allocate soil properties to different classification schemes.
}
\usage{
allocate(..., to = NULL, droplevels = TRUE)
codify(x, droplevels = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{further arguments, usually a named vector of soil properties (e.g. pH = 1:14)}
  \item{to}{character specifying the classification scheme}
  \item{droplevels}{logical indicating whether to drop unused levels in factors. This is useful when the results have a large number of unused classes, which can waste space in tables and figures.}
}
\details{
This function is intended to allocate a set of soil properties to an established soil classification scheme, such as Salt Severity or Black Soil. Allocation is often confused with classification. While classification is the 'act' of developing a grouping scheme, allocation is the assignment or identification of measurements to a class (Webster and Oliver, 1990). 

Often raster data cannot be stored as a character and thus needs to be stored as an integer. Thus, the codify() function is intended to convert integer values back to a factor.}
\value{
a vector or data frame of equal length to the inputs of ...}
\references{
Abrol, I., Yadav, J. & Massoud, F. 1988. \href{http://www.fao.org/3/x5871e/x5871e00.htm}{Salt-affected soils and their management}. No. Bulletin 39. Rome, FAO Soils.

FAO. 2006. \href{http://www.fao.org/publications/card/en/c/903943c7-f56a-521a-8d32-459e7e0cdae9/}{Guidelines for soil description}. Rome, Food and Agriculture Organization of the United Nations.

FAO. 2020. DEFINITION | What is a black soil? [online]. [Cited 28 December 2020]. http://www.fao.org/global-soil-partnership/intergovernmental-technical-panel-soils/gsoc17-implementation/internationalnetworkblacksoils/more-on-black-soils/definition-what-is-a-black-soil/es/

Richards, L.A. 1954. \href{https://www.ars.usda.gov/ARSUserFiles/20360500/hb60_pdf/hb60complete.pdf}{Diagnosis and Improvement of Saline and Alkali Soils}. U. S. Government Printing Office. 166 pp.

Webster, R., Oliver, M.A., 1990. Statistical Methods in Soil and Land Resource Survey, Spatial Information Systems. Oxford University Press, New York.
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
# Salt Severity
test <- expand.grid(
  EC  = sort(sapply(c(0, 0.75, 2, 4, 8, 15, 30), function(x) x + c(0, -0.05, 0.05))),
  pH  = c(8.1, 8.2, 8.3, 8.4, 8.5, 8.6),
  ESP = sort(sapply(c(0, 15, 30, 50, 70, 100), function(x) x + c(0, 0.1, -0.1)))
)
test$ss      <- with(test, allocate(EC = EC, pH = pH, ESP = ESP, to = "Salt Severity"))
test$ss_code <- as.integer(test$ss)
test$ss2     <- codify(test$ss_code)
all(test$ss == test$ss2)


# Black Soil Category 1 (BS1)
test <- expand.grid(
  dept = seq(0, 50, 10),
  OC   = sort(sapply(c(0, 0.6, 1.2, 20, 40), function(x) x + c(0, -0.05, 0.05))),
  chroma_moist  = 2:4,
  value_moist   = 2:4,
  value_dry     = 4:6,
  thickness     = 24:26,
  CEC           = 24:26,
  BS            = 49:51,
  tropical      = c(TRUE, FALSE)
)
test$pedon_id <- rep(1:21870, each = 6)
test$depb     <- test$dept + 10

bs1 <- with(test, allocate(OC, chroma_moist, value_moist, value_dry, thickness, CEC, BS, to = "FAO Black Soil"))
test2 <- cbind(test, bs1)

## summarize BS1 by pedon
test_seg  <- segment(test2, intervals = c(0, 25), hzdepcols = c("dept", "depb"))
test_prof <- aggregate(BS1 ~ pedon_id, data = test_seg, FUN = all, na.action = na.pass)


# Black Soil Category 2
bs2 <- with(test, allocate(OC, chroma_moist, value_moist, value_dry, thickness, to = "Black Soil", horizon = FALSE))
test2 <- cbind(test, bs2)
idx <- test2$BS2 == TRUE
summary(test2[idx, ])


# Tropical Black Soil
tbs <- with(test, allocate(OC, chroma_moist, value_moist, value_dry, thickness, CEC, BS, tropical = TRUE, to = "Black Soil", horizon = FALSE))
test2 <- cbind(test, tbs)
idx <- test2$BS2 == TRUE
summary(test2[idx, ])
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
