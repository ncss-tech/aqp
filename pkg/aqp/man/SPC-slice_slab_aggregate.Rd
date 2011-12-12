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
data(sp1)
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
depths(sp1) <- id ~ top + bottom	
s <- slice(sp1, 10 ~ prop + name + soil_color)

}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
