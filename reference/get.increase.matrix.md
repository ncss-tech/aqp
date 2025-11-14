# Compute Pair-wise Distances of Soil Properties over Depth

Computes pair-wise distance matrix to determine where an attribute
increases within a specified vertical distance threshold.

`get.increase.depths` performs the conversion of the square matrix
output of `get.increase.matrix` back to horizon top depth for where
criteria were met.

## Usage

``` r
get.increase.matrix(p, attr, threshold.fun, vertical.distance)

get.increase.depths(p, attr, threshold.fun, vertical.distance)
```

## Arguments

- p:

  a SoilProfileCollection, containing a single profile

- attr:

  horizon attribute name to get the "increase" of

- threshold.fun:

  a function that returns the threshold (as a function of attr); may
  return a constant single value

- vertical.distance:

  the vertical distance (determined from difference SPC top depth
  variable) within which increase must be met

## Value

Returns a square logical matrix reflecting where the increase criteria
were met.

`get.increase.depths` converts to horizon top depth by using above
matrix output to determine depths where increase is met.

Returns a numeric vector of depths where the increase requirement is
met. For the argillic, the first is the one of interest.

`get.increase.depths()` converts to horizon top depth by using above
matrix output to determine depths where increase is met.

## Details

Uses matrix outer product to determine all pair-wise differences in
`attr` for the horizons of `p`. Supplies `attr` to `threshold.fun` to
determine the minimum value criterion to return TRUE in output matrix
for an "increase". Also, computes all pair-wise distances in depth
dimension to determine whether the vertical distance criteria have been
met simultaneously with `attr` increase.

This function assumes that the `threshold.fun` supplied by the user
returns either a constant or a vector of equal length to its input.

Note that the `threshold.fun` result is allowed to contain NA, but that
will result in no output for affected cells.

`get.increase.depths()` performs the conversion of the square matrix
output of `get.increase.matrix` back to horizon top depth for where
criteria were met.

Note that the `threshold.fun` result is allowed to contain `NA`, but
that will result in no output for affected cells.

## See also

[`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md),
[`crit.clay.argillic()`](https://ncss-tech.github.io/aqp/reference/crit.clay.argillic.md)

[`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md)
[`crit.clay.argillic()`](https://ncss-tech.github.io/aqp/reference/crit.clay.argillic.md)

## Author

Andrew Gene Brown

## Examples

``` r
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents
foo <- get.increase.matrix(p, threshold.fun = crit.clay.argillic,
                           attr = attr, vertical.distance = 30)
foo
#>       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]
#> [1,] FALSE FALSE FALSE FALSE FALSE FALSE
#> [2,] FALSE FALSE FALSE  TRUE FALSE FALSE
#> [3,] FALSE FALSE FALSE  TRUE  TRUE FALSE
#> [4,] FALSE FALSE FALSE FALSE  TRUE FALSE
#> [5,] FALSE FALSE FALSE FALSE FALSE FALSE
#> [6,] FALSE FALSE FALSE FALSE FALSE FALSE


data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
#> This is already a SoilProfileCollection-class object, doing nothing.
site(sp1) <- ~ group
#> Error in eval(predvars, data, env): object 'group' not found

p <- sp1[1]
attr <- 'prop' # clay contents
foo <- get.increase.depths(p, threshold.fun = crit.clay.argillic,
                           attr = attr, vertical.distance = 30)
foo
#> [1] 49 57
```
