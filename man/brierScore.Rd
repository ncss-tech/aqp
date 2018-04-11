\name{brierScore}
\alias{brierScore}

\title{Multinominal Brier score}
\description{Compute the multinominal Brier score}

\usage{
brierScore(x, classLabels, actual = "actual")
}

\arguments{
  \item{x}{\code{data.frame}, rows are predictions/observations, columns contain classes}
  \item{classLabels}{vector of predicted class labels (probabilities), corrosponding to column names in \code{x}}
  \item{actual}{name of column containing the observed class}
}

\value{A single numeric value.}

\references{
Brier, GLenn W. 1950. "Verification of Forecasts Expressed in Terms of Probability." Monthly Weather Review 78 (1): 1-3. doi:10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2.
}

\author{D.E. Beaudette}

\keyword{ manip }

