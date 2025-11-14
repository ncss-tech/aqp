# Guess Appropriate Ordering for Generalized Horizon Labels

This function makes an (educated) guess at an appropriate set of levels
for generalized horizon labels using the median of horizon depth
mid-points.

## Usage

``` r
guessGenHzLevels(x, hz = GHL(x, required = TRUE))
```

## Arguments

- x:

  a `SoilProfileCollection` object

- hz:

  name of horizon-level attribute containing generalized horizon labels,
  see details

## Value

a list:

- levels:

  a vector of levels sorted by median horizon depth mid-point

- median.depths:

  a vector of median horizon mid-points

## Details

This function is useful when groups of horizons have been generalized
via some method other than `generalize.hz`. For example, it may be
useful to generalize horizons using labels derived from slice depths.
The default sorting of these labels will not follow a logical depth-wise
sorting when converted to a factor. `guessGenHzLevels` does a good job
of "guessing" the proper ordering of these labels based on median
horizon depth mid-point.

## See also

[`generalize.hz`](https://ncss-tech.github.io/aqp/reference/generalize.hz.md)

## Author

D.E. Beaudette

## Examples

``` r
# load some example data
data(sp1, package='aqp')

# upgrade to SoilProfileCollection
depths(sp1) <- id ~ top + bottom

# generalize horizon names
n <- c('O', 'A', 'B', 'C')
p <- c('O', 'A', 'B', 'C')
sp1$genhz <- generalize.hz(sp1$name, n, p)

# note: levels are in the order in which originally defined:
levels(sp1$genhz)
#> [1] "O"        "A"        "B"        "C"        "not-used"

# generalize horizons by depth slice
s <- dice(sp1, c(5, 10, 15, 25, 50, 100, 150) ~ .)
#> dropping horizons with invalid depth logic, see `metadata(x)$removed.horizons`
#> filling gaps left by HzDepthLogicSubset
s$slice <- paste0(s$top, ' cm')
# not a factor
levels(s$slice)
#> NULL

# the proper ordering of these new labels can be guessed from horizon depths
guessGenHzLevels(s, 'slice')
#> $levels
#> [1] "5 cm"   "10 cm"  "15 cm"  "25 cm"  "50 cm"  "100 cm" "150 cm"
#> 
#> $median.depths
#>   5 cm  10 cm  15 cm  25 cm  50 cm 100 cm 150 cm 
#>    5.5   10.5   15.5   25.5   50.5  100.5  150.5 
#> 

# convert to factor, and set proper order
s$slice <- factor(s$slice, levels=guessGenHzLevels(s, 'slice')$levels)

# that is better
levels(s$slice)
#> [1] "5 cm"   "10 cm"  "15 cm"  "25 cm"  "50 cm"  "100 cm" "150 cm"
```
