# Replace Data in Horizon Slot

Replaces horizon data with new `data.frame` object.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
replaceHorizons(object) <- value
```

## Arguments

- object:

  A `SoilProfileCollection`

- value:

  An object inheriting `data.frame`

## Examples

``` r
# load test data
data(sp2)

# promote to SPC
depths(sp2) <- id ~ top + bottom

# one profile
p <- sp2[1,]

# 23 variables in horizon data
length(horizonNames(sp2))
#> [1] 23

# remove all but essential ones
replaceHorizons(p) <- horizons(p)[,c(idname(p), hzidname(p), horizonDepths(p))]

# inspect result (a clean slate)
horizons(p)
#>      id hzID top bottom
#> 1 hon-1    1   0     12
#> 2 hon-1    2  12     16
#> 3 hon-1    3  16     25
#> 4 hon-1    4  25     49
#> 5 hon-1    5  49     85
#> 6 hon-1    6  85     95
#> 7 hon-1    7  95    122
#> 8 hon-1    8 122    140
#> 9 hon-1    9 140    170
```
