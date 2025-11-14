# Check a SoilProfileCollection object for errors in horizon depths.

This function inspects a `SoilProfileCollection` object, looking for
four common errors in horizon depths:

1.  bottom depth shallower than top depth

2.  equal top and bottom depth

3.  missing top or bottom depth (e.g. `NA`)

4.  gap or overlap between adjacent horizons (only if `byhz = FALSE`)

## Usage

``` r
checkHzDepthLogic(
  x,
  hzdepths = NULL,
  idname = NULL,
  fast = FALSE,
  byhz = FALSE
)
```

## Arguments

- x:

  `SoilProfileCollection` or `data.frame` object to check

- hzdepths:

  character vector, describing top and bottom depths in a
  `SoilProfileCollection` or `data.frame`. `horizonDepths(x)` is used
  when `x` is a `SoilProfileCollection`.

- idname:

  character, describing the column containing profile IDs in a
  `SoilProfileCollection` or `data.frame`. `idname(x)` is used when `x`
  is a `SoilProfileCollection`.

- fast:

  logical, When `TRUE`, details about specific test results are not
  needed, the operation can allocate less memory and run approximately
  5x faster.

- byhz:

  logical, apply logic tests to profiles (`FALSE`) or individual
  horizons (`TRUE`)?

## Value

A `data.frame` containing profile IDs, validity boolean (`valid`) and
test results if `fast = FALSE`.

The `data.frame` will have as many rows as profiles in `x`
(`length(x)`).

- `id` : Profile IDs, named according to `idname(x)`

- `valid` : boolean, profile passes all of the following tests

- `depthLogic` : boolean, errors related to depth logic

- `sameDepth` : boolean, errors related to same top/bottom depths

- `missingDepth` : boolean, NA in top / bottom depths

- `overlapOrGap` : boolean, gaps or overlap in adjacent horizons (`NA`
  when `byhz = TRUE`)

## Author

D.E. Beaudette, A.G. Brown, S.M. Roecker

## Examples

``` r
## sample data

data(sp3)
depths(sp3) <- id ~ top + bottom

# these data should be clean
res <- checkHzDepthLogic(sp3)

head(res)
#>   id valid depthLogic sameDepth missingDepth overlapOrGap
#> 1  1  TRUE      FALSE     FALSE        FALSE        FALSE
#> 2 10  TRUE      FALSE     FALSE        FALSE        FALSE
#> 3  2  TRUE      FALSE     FALSE        FALSE        FALSE
#> 4  3  TRUE      FALSE     FALSE        FALSE        FALSE
#> 5  4  TRUE      FALSE     FALSE        FALSE        FALSE
#> 6  5  TRUE      FALSE     FALSE        FALSE        FALSE

# less memory if only concerned about net validity
res <- checkHzDepthLogic(sp3, fast = TRUE)

head(res)
#>   id valid
#> 1  1  TRUE
#> 2 10  TRUE
#> 3  2  TRUE
#> 4  3  TRUE
#> 5  4  TRUE
#> 6  5  TRUE
```
