\name{sp3}
\Rdversion{1.1}
\alias{sp3}
\docType{data}
\title{
Soil Profile Data Example 3
}
\description{
Soil samples from 10 soil profiles, taken from the Sierra Foothill Region of California.
}
\usage{data(sp3)}
\format{
  A data frame with 46 observations on the following 15 variables.
  \describe{
    \item{\code{id}}{soil id}
    \item{\code{top}}{horizon upper boundary (cm)} 
    \item{\code{bottom}}{horizon lower boundary (cm)}
    \item{\code{clay}}{clay content}
    \item{\code{cec}}{CEC by amonium acetate at pH 7}
    \item{\code{ph}}{pH in 1:1 water-soil mixture}
    \item{\code{tc}}{total carbon percent}
    \item{\code{hue}}{Munsell hue (dry)}
    \item{\code{value}}{Munsell value (dry)}
    \item{\code{chroma}}{Munsell chroma (dry)}
    \item{\code{mid}}{horizon midpoint (cm)}
    \item{\code{ln_tc}}{natural log of total carbon percent}
    \item{\code{L}}{color: l-coordinate, CIE-LAB colorspace (dry)}
    \item{\code{A}}{color: a-coordinate, CIE-LAB colorspace (dry)}
    \item{\code{B}}{color: b-coordinate, CIE-LAB colorspace (dry)}
    \item{name}{horizon name}
    \item{soil_color}{horizon color}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(sp3)
## maybe str(sp3) ; plot(sp3) ...
}
\keyword{datasets}
