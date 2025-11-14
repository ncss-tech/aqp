# Test for a valid `SoilProfileCollection`

Test for a valid `SoilProfileCollection`

## Usage

``` r
checkSPC(x)
```

## Arguments

- x:

  a `SoilProfileCollection` object

## Value

TRUE or FALSE. Consider using
[`rebuildSPC()`](https://ncss-tech.github.io/aqp/reference/rebuildSPC.md)
if FALSE.

## Details

Test for valid `SoilProfileCollection` by checking for slots defined in
the class prototype. Likely only used between major versions of `aqp`
where internal structure of `SoilProfileCollection` has changed. Use
`checkHzDepthLogic` to check for common errors in horizon depths.

## See also

[`rebuildSPC`](https://ncss-tech.github.io/aqp/reference/rebuildSPC.md),
[`checkHzDepthLogic`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md)

## Author

D.E. Beaudette
