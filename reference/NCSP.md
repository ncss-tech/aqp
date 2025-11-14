# Numerical Classification of Soil Profiles

Replacement for `profile_compare()`.

Performs a numerical comparison of soil profiles using named properties,
based on a weighted, summed, depth-segment-aligned dissimilarity
calculation.

Variability in soil depth can interfere significantly with the
calculation of between-profile dissimilarity–what is the numerical
“distance” (or dissimilarity) between a slice of soil from profile A and
the corresponding, but missing, slice from a shallower profile B?
Gower's distance metric would yield a NULL distance, despite the fact
that intuition suggests otherwise: shallower soils should be more
dissimilar from deeper soils. For example, when a 25 cm deep profile is
compared with a 50 cm deep profile, numerical distances are only
accumulated for the first 25 cm of soil (distances from 26 - 50 cm are
NULL). When summed, the total distance between these profiles will
generally be less than the distance between two profiles of equal depth.
Our algorithm will replace NULL distances with the maximum distance
between any pair of profiles for the current depth slice. In this way,
the numerical distance between a slice of soil and a corresponding slice
of non-soil reflects the fact that these two materials should be treated
very differently.

This alternative calculation of dissimilarities between soil and
non-soil slices solves the problem of comparing shallow profiles with
deeper profiles. However, it can result in a new problem: distances
calculated between two shallow profiles will be erroneously inflated
beyond the extent of either profile's depth. Our algorithm will preserve
NULL distances between slices when both slices represent non-soil
material. Therefore, shallow profiles will only accumulate mutual
dissimilarity to the depth of the deeper profile.

Slices are classified as 'soil' down to the maximum depth to which at
least one of variables used in the dissimilarity calculation is not NA.
This will cause problems when profiles within a collection contain all
NAs within the columns used to determine dissimilarity. An approach for
identifying and removing these kind of profiles is presented in the
examples section below.

Our approach builds on the work of (Moore, 1972) and the previously
mentioned depth-slicing algorithm. See references below for a detailed
explanation of the NCSP algorithm.

## Usage

``` r
NCSP(
  x,
  vars,
  fm = NULL,
  weights = rep(1, times = length(vars)),
  maxDepth = max(x),
  k = 0,
  isColor = FALSE,
  rescaleResult = FALSE,
  verbose = TRUE,
  returnDepthDistances = FALSE
)
```

## Arguments

- x:

  `SoilProfileColection` object, should be pre-filtered to remove
  profiles with horizon depth logic, see
  [`HzDepthLogicSubset`](https://ncss-tech.github.io/aqp/reference/HzDepthLogicSubset.md)

- vars:

  character vector, names of horizon attributes to use in the
  classification

- fm:

  formula, formula as specified to
  [`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md),
  not yet implemented

- weights:

  numeric vector, same length as `vars`: variable importance weights,
  need not sum to 1

- maxDepth:

  numeric, maximum depth of analysis

- k:

  numeric, weighting coefficient, usually between 0-1. A value of 0
  results in no depth-weighting. See examples.

- isColor, :

  logical: variables represent color, should be CIELAB coordinates (D65
  illuminant), weights are ignored. Variables should be named `L`, `A`,
  `B` in specified in that order.

- rescaleResult:

  logical, distance matrix is rescaled based on max(D)

- verbose:

  logical, extra output messages

- returnDepthDistances:

  logical, return a list of distances by depth slice

## Note

`NCSP()` will overwrite the `removed.profiles` metadata from `x`.

## References

- J.J Maynard, S.W. Salley, D.E. Beaudette, J.E Herrick. Numerical soil
  classification supports soil identification by citizen scientists
  using limited, simple soil observations. Soil Sci. Soc. Am. J. 2020;
  84: 1675-1692.
  [doi:10.1002/saj2.20119](https://doi.org/10.1002/saj2.20119) .

- D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for quantitative
  pedology: A toolkit for soil scientists, Computers & Geosciences,
  Volume 52, 2013, Pages 258-268, ISSN 0098-3004,
  [doi:10.1016/j.cageo.2012.10.020](https://doi.org/10.1016/j.cageo.2012.10.020)
  .

- Moore, A.; Russell, J. & Ward, W. Numerical analysis of soils: A
  comparison of three soil profile models with field classification.
  Journal of Soil Science, 1972, 23, 194-209.

## See also

[`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md),
[`cluster::daisy()`](https://rdrr.io/pkg/cluster/man/daisy.html),
[`compareSites()`](https://ncss-tech.github.io/aqp/reference/compareSites.md)

## Author

Dylan E. Beaudette and Jon Maynard
