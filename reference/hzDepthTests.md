# Tests of horizon depth logic

Function used internally by
[`checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md),
[`glom()`](https://ncss-tech.github.io/aqp/reference/glom.md) and
various other functions that operate on horizon data from single soil
profiles and require a priori depth logic checks. Checks for bottom
depths less than top depth / bad top depth order ("depthLogic"), bottom
depths equal to top depth ("sameDepth"), overlaps/gaps ("overlapOrGap")
and missing depths ("missingDepth"). Use `names(res)[res]` on result
`res` of `hzDepthTest()` to to determine type of logic error(s) found –
see examples below.

## Usage

``` r
hzDepthTests(top, bottom = NULL)
```

## Arguments

- top:

  A numeric vector containing horizon top depths. Or a `data.frame` with
  two columns (first containing top depths, second containing bottom)

- bottom:

  A numeric vector containing horizon bottom depths.

## Value

A named logical vector containing TRUE for each type of horizon logic
error found in the given data.

## Author

Andrew G. Brown & Dylan E. Beaudette

## Examples

``` r
# no logic errors
res <- hzDepthTests(top = c(0,10,20,30), bottom = c(10,20,30,50))
names(res)[res]
#> character(0)

# bottom < top
hzDepthTests(top = c(10,20,30,50), bottom = c(0,10,20,30))
#>   depthLogic    sameDepth missingDepth overlapOrGap 
#>         TRUE        FALSE        FALSE         TRUE 
names(res)[res]
#> character(0)

# bottom == top
hzDepthTests(top = c(10,20,30,50), bottom = c(0,20,20,30))
#>   depthLogic    sameDepth missingDepth overlapOrGap 
#>         TRUE         TRUE        FALSE         TRUE 
names(res)[res]
#> character(0)

# overlap
hzDepthTests(top = c(0,5,20,30), bottom = c(10,20,30,50))
#>   depthLogic    sameDepth missingDepth overlapOrGap 
#>        FALSE        FALSE        FALSE         TRUE 
names(res)[res]
#> character(0)

# gap
hzDepthTests(top = c(0,15,20,30), bottom = c(10,20,30,50))
#>   depthLogic    sameDepth missingDepth overlapOrGap 
#>        FALSE        FALSE        FALSE         TRUE 
names(res)[res]
#> character(0)

# missing
hzDepthTests(c(0,15,NA,30),c(10,NA,30,50))
#>   depthLogic    sameDepth missingDepth overlapOrGap 
#>         TRUE        FALSE         TRUE         TRUE 
names(res)[res]
#> character(0)
```
