# Return upper boundary of argillic horizon

Returns the top depth of the argillic horizon as a numeric vector.

## Usage

``` r
argillic.clay.increase.depth(p, clay.attr = "clay")
```

## Arguments

- p:

  A single-profile `SoilProfileCollection` object.

- clay.attr:

  OPTIONAL: horizon attribute name referring to clay content. default:
  `clay`

## Value

A numeric vector containing top depth of argillic horizon, if present,
or NA.

## Details

Uses `crit.clay.argillic` to determine threshold clay increase, and
`get.increase.matrix` to determine where increase is met within a
vertical distance of 30 cm.

## See also

`getArgillicBounds`, `get.increase.matrix`, `crit.clay.argillic`

## Author

Andrew Gene Brown

## Examples

``` r
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents 
foo <- argillic.clay.increase.depth(p, clay.attr = attr)
foo
#> [1] 49
```
