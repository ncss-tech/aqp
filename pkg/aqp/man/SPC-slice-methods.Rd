\name{slice-methods}
\docType{methods}
\alias{get.slice}
\alias{slice}
\alias{slice,SoilProfileCollection-method}
\title{ ~~ Methods for Function \code{slice} in Package \pkg{aqp} ~~}
\description{
 ~~ Methods for function \code{slice} in package \pkg{aqp} ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{
%%  ~~describe this method here~~
}
}}
\examples{
# simulate some data, IDs are 1:20
d <- ldply(1:20, random_profile)

# re-order IDs
new.order <- sample(1:20)
head(d <- d[order(match(d$id, new.order), d$top), ])

# init SoilProfilecollection object: IDs are reset according to natural order of ID labels
depths(d) <- id ~ top + bottom
head(horizons(d))

# check order of IDs: these match the re-ordered horizon data
profile_id(d)

# slice-up at 10 cm: these IDs match the output from profile_id()
# output a data.frame instead of a SoilProfilecollection
s <- slice(d, 10 ~ p1 + p2 + p3, just.the.data=TRUE)

# are the results in the same order as our original IDs?
if( ! all.equal(s$id, profile_id(d)))
  stop('IDs do not match')
}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
