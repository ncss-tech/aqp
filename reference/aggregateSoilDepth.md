# Probabilistic Estimation of Soil Depth within Groups

Estimate the most-likely depth to contact within a collection of soil
profiles. Consider `getSoilDepthClass` followed by group-wise percentile
estimation as a faster alternative.

## Usage

``` r
aggregateSoilDepth(
  x,
  groups,
  crit.prob = 0.9,
  name = hzdesgnname(x),
  p = "Cr|R|Cd",
  ...
)
```

## Arguments

- x:

  a `SoilProfileCollection` object

- groups:

  the name of a site-level attribute that defines groups of profiles
  within a collection

- crit.prob:

  probability cutoff used to determine where the most likely depth to
  contact will be, e.g. 0.9 translates to 90% of profiles are shallower
  than this depth

- name:

  horizon-level attribute where horizon designation is stored, defaults
  to `hzdesgnname(x)`

- p:

  a REGEX pattern that matches non-soil genetic horizons

- ...:

  additional arguments to `slab`

## Value

A `data.frame` is returned, with as many rows as there are unique group
labels, as specified in `groups`.

## Details

This function computes a probability-based estimate of soil depth by
group. If no grouping variable exists, a dummy value can be used to
compute a single estimate. The `crit.prob` argument sets the critical
probability (e.g. 0.9) at which soil depth within a group of profiles is
determined. For example, a `crit.prob` of 0.95 might result in an
estimated soil depth (e.g. 120cm) where 95% of the profiles (by group)
had depths that were less than or equal to 120cm.

## See also

[`estimateSoilDepth()`](https://ncss-tech.github.io/aqp/reference/estimateSoilDepth.md)
[`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md)

## Author

D.E. Beaudette

## Examples

``` r
data(sp1)
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# set horizon designation in SPC
hzdesgnname(sp1) <- 'name'

aggregateSoilDepth(sp1, 'group', crit.prob = 0.9)
#> horizons with zero thickness have been omitted from results
#>   group soil.top soil.bottom
#> 1     1        0         232
#> 2     2        0          67
```
