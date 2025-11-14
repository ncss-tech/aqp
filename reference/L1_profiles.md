# Create Representative Soil Profiles via L1 Estimator

The L1 estimator, or [geometric
median](https://en.wikipedia.org/wiki/Geometric_median), is a
multivariate generalization of the (univariate)
[median](https://en.wikipedia.org/wiki/Median) concept. This function
performs a multivariate aggregation (via L1 estimator) according to a
suite of ratio-scale soil properties. The L1 estimator is applied to
soil profile data that have been sliced to a 1-depth-unit basis. Data
should be well stratified by groups defined in `fm`, otherwise the L1
median may not make any sense.

See the [L1 Profiles
Tutorial](https://ncss-tech.github.io/AQP/aqp/L1-profiles.html) for
additional examples.

## Usage

``` r
L1_profiles(
  x,
  fm,
  basis = 1,
  method = c("regex", "simple", "constant"),
  maxDepthRule = c("max", "min"),
  maxDepthConstant = NULL
)
```

## Arguments

- x:

  `SoilProfileCollection` object

- fm:

  formula, for example: `group ~ p1 + p2 + p3`, where "group" is a
  site-level grouping variable, and "p1", "p2", and "p3" are horizon
  level variables

- basis:

  positive integer, aggregation basis (e.g. 1 for 1-depth-unit
  intervals). Values other than 1 are not currently supported.

- method:

  soil depth evaluation method: "regex" for regular expression,
  "simple", or "constant". See details.

- maxDepthRule:

  maximum depth rule: "max" or "min" See details.

- maxDepthConstant:

  positive integer, maximum depth when `maxDepthRule = 'constant'`

## Value

a `SoilProfileCollection` object

## Details

See [this related
tutorial](https://ncss-tech.github.io/AQP/aqp/L1-profiles.html) for
additional examples. The `method`, `maxDepthRule`, and
`maxDepthConstant` arguments set the maximum depth (over the entire
collection) of analysis used to build "L1 profiles". The following rules
are available:

- `method = 'regex'` uses pattern matching on horizon designations (note
  that `hzdesgnname` metadata must be set with
  `hzdesgnname(x) <- 'columnname'`)

- `method = 'simple'` uses `min` or `max` as applied to `x`, no
  accounting for non-soil horizons (e.g. Cr or R)

- `method = 'constant'` uses a fixed depth value supplied by
  `maxDepthConstant`

The `maxDepthRule` argument sets depth calculation constraint, applied
to soil depths computed according to `method` (`min` or `max`).

## Note

This function requires the `Gmedian` package.

## References

Cardot, H., Cenac, P. and Zitt, P-A. (2013). Efficient and fast
estimation of the geometric median in Hilbert spaces with an averaged
stochastic gradient algorithm. Bernoulli, 19, 18-43.
