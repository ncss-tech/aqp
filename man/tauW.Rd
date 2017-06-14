\name{tau}
\Rdversion{1}
\alias{tauW}
\alias{summaryTauW}
\alias{xtableTauW}

\title{
Compute weighted naive and \emph{tau} statistics for a cross-classification matrix
}

\description{
\code{tauW}: Computes: (1) unweighted naive, (2) weighted naive, (3) unweighted \emph{tau}, (4) weighted \emph{tau} accuracy statistics

\code{summaryTauW}: prints a summary of the results from \emph{tauW}

\code{xtableTauW}: formats a LaTeX table with results from \emph{tauW} and saves it as a \code{.tex} file for import into a LaTeX document.
}
\usage{
tauW(CM, W = diag(sqrt(length(as.matrix(CM)))), 
P = rep(1/nrow(as.matrix(CM)), nrow(as.matrix(CM))))

summaryTauW(result.tau)

xtableTauW(result.tau, file.name="tau_results_table.tex")
}
\arguments{
  \item{CM}{a square confusion (cross-classification) matrix (rows: allocation, columns: reference)}
  \item{W}{weights: 1 on diagonals, [-1..1] off giving partial credit to this error}
  \item{P}{prior probability vector, length = number of rows/columns in \code{CM} and \code{W}}
  \item{result.tau}{results returned by \code{tauW}}
  \item{file.name}{output LaTeX file}
}

\details{

Input matrices \code{CM} and \code{W} may be in \code{data.frame} format and will be converted

Weights matrix \code{W}:  0 = no credit; 1 = full credit; -1 = maximum penalty/ If absent, default is no partial credit, i.e., unweighted.

Prior probabilities vector \code{P}: If absent, \code{P} are equal priors for each class. Special value \code{P = 0} is interpreted as \code{P} = column marginals.

Error checks: \code{CM} must be square; \code{P} must have correct number of classes and sum to 1 +/- 0.0001; \code{W} & \code{CM} must be conformable
}

% \note{}

\value{
Results are returned in a list with obvious R names
}

\references{
Rossiter, D. G., Zeng, R., & Zhang, G.-L. (2017). \emph{Accounting for taxonomic distance in accuracy assessment of soil class predictions}. \bold{Geoderma}, 292, 118--127. \url{https://doi.org/10.1016/j.geoderma.2017.01.012}

Ma, Z. K., & Redmond, R. L. (1995). \emph{Tau-coefficients for accuracy assessment of classification of remote-sensing data}. \bold{Photogrammetric Engineering and Remote Sensing}, 61(4), 435--439.

Naesset, E. (1996). \emph{Conditional tau coefficient for assessment of producer's accuracy of classified remotely sensed data}. \bold{ISPRS Journal of Photogrammetry and Remote Sensing}, 51(2), 91--98. \url{http://dx.doi.org/10.1016/0924-2716(69)00007-4}
}
\author{D G Rossiter}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{array}
