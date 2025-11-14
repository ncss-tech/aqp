# Determines threshold (minimum) clay content for argillic upper bound

Given a vector or matrix of "eluvial" horizon clay contents (\\
`crit.clay.argillic()` returns a vector or matrix of minimum clay
contents (thresholds) that must be met for an argillic horizon clay
increase.

## Usage

``` r
crit.clay.argillic(eluvial_clay_content)
```

## Arguments

- eluvial_clay_content:

  A numeric vector or matrix containing clay contents of potential
  "eluvial" horizons. May contain `NA`.

## Value

A vector or matrix (input-dependent) containing minimum "illuvial"
horizon clay contents (thresholds) to be met for argillic horizon clay
increase.

## Details

Uses the standard equations for clay contents less than 15 \\ and 40 \\
the definition of the argillic horizon from 12th Edition Keys to Soil
Taxonomy (Soil Survey Staff, 2014).

## Note

This function is intended for identifying clay content threshold
required for an argillic horizon. These thresholds may not apply
depending on the specifics of your soil. E.g. if the upper part of
argillic has been plowed (has Ap immediately over upper boundary) the
clay increase requirement can be waived (Soil Survey Staff, 2014).

## References

Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural
Resources Conservation Service, Washington, DC.

## See also

[`getArgillicBounds`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md),
[`get.increase.matrix`](https://ncss-tech.github.io/aqp/reference/get.increase.matrix.md)

## Author

Andrew Gene Brown

## Examples

``` r
# crit.clay.argillic uses different equations for clay content
# less than 15 %, between 15 and 40 %, and >40 %

crit.clay.argillic(eluvial_clay_content=c(5, 20, 45))
#> [1]  8 24 53
```
