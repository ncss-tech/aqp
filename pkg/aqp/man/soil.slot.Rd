\name{soil.slot}
\Rdversion{1.1}
\alias{soil.slot}
\alias{seg.summary}

\title{Slice-Wise Aggregation of Soil Properties}
\description{Align a single soil property to a user-defined basis, and perform slice-wise aggregation.}
\usage{
soil.slot(data, seg_size = NA, seg_vect = NA, 
use.wts = FALSE, strict = FALSE, user.fun = NULL, class_prob_mode=1)
}

\arguments{
  \item{data}{
A dataframe representing a 'stack' of soil profiles and having the following format:
	\describe{
		\item{id}{An id that is unique across soil profiles within the dataframe.}
		\item{top}{The horizon top boundary, must be an integer.}
		\item{bottom}{The horizon bottom boundary, must be an integer.}
		\item{prop}{A property to be aggregated. can be numeric or a factor.}		
	}
}
  \item{seg_size}{User-degined segment size, default is 1.}
  
  \item{seg_vect}{User-degined segment structure: should start from 0, and deepest boundary should be deeper than the deepest soil profile in the collection. For example, if the deepest profile in the collection is 200 cm, then the following segmenting vector would be reasonable: \code{c(0,10,20,30,60,100,150,210)}. The resulting aggregation will automatically be truncated at 200 cm. The user is responsible for supplying sensible values.}
  
  \item{use.wts}{If TRUE, then a column called 'wt' should be present in the source dataframe, and must make sense within the context of the data (i.e. area weights, etc.). Weighted means, standard deviations, quantiles, and optionally proportions will be returned by depth-slice. See detailes and examples below.}
  
\item{strict}{should horizons be strictly checked for self-consistency? defaults to FALSE}

\item{user.fun}{User-defined function that should accept a vector and return a scalar. This function should know how to properly deal with unexpected, NA, NULL, or Inf values and trap them accordingly.}

\item{class_prob_mode}{Strategy for normalizing slice-wise probabilities, dividing by either: number of profiles with data at the current slice (class_prob_mode=1), or by the number of profiles in the collection (class_prob_mode=2). Mode 2 values will always sum to the contributing fraction, while mode 1 values will always sum to 1. Mode 2 is likely the best way to communicate horizon probability near the lower range in soil depth within a collection of soil profiles.}
}

\details{
Unweighted and weighted summary stats are computed with the Hmisc functions \code{wtd.mean}, \code{wtd.var}, and \code{wtd.quantile}. Weighted probabilities (proportions) will be implemented in a future release. See the sample dataset 'sp1' documentation for further examples on how to used \code{soil.slot}. Basic error checking is performed to make sure that bottom and top horizon boundaries make sense. Note that the horizons should be sorted according to depth before using this function.

Data are returned according to the following:
A dataframe with slice-wise aggregation. When \code{prop} is numeric a dataframe is returned in the following format:
	\describe{
		\item{top}{The slice top boundary.}
		\item{bottom}{The slice bottom boundary.}
		\item{contributing_fraction}{The fraction of profiles contributing to the aggregate value, ranges from 1/n_profiles to 1.}
		\item{p.mean}{The slice-wise (optionally weighted) mean.}
		\item{p.sd}{An slice-wise (optionally weighted) standard deviation.}
		\item{p.q5}{The slice-wise 5th percentile.}
		\item{p.q25}{The slice-wise 25th percentile}
		\item{p.q50}{The slice-wise 50th percentile (median)}
		\item{p.q75}{The slice-wise 75th percentile}
		\item{p.q95}{The slice-wise 95th percentile}
	}

When \code{prop} is a factor variable, slice-wise probabilities for each level of \code{prop}:
	\describe{
		\item{top}{The slice top boundary.}
		\item{bottom}{The slice bottom boundary.}
		\item{contributing_fraction}{The fraction of profiles contributing to the aggregate value, ranges from 1/n_profiles to 1.}
		\item{A}{The slice-wise probability of level A}
		\item{B}{The slice-wise probability of level B}
		\item{\dots}{}
		\item{n}{The slice-wise probability of level n}
	}

}

\note{If a user-defined function (\code{user.fun}) is specified, care must be taken if sample size is used within the calculation _and_ slice sizes are > 1 depth unit.}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{D.E. Beaudette}
\note{This function is used internally by other functions. Examples below should be used with caution, as they will be migrated into higher-level documentation soon.}

\seealso{ \code{\link{sp1}}, \code{\link{slab}} }

\keyword{manip}
