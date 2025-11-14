# Flag perfectly overlapping horizons within a SoilProfileCollection

Flag perfectly overlapping horizons within a SoilProfileCollection

## Usage

``` r
flagOverlappingHz(x)
```

## Arguments

- x:

  a `SoilProfileCollection` object

## Value

logical vector with length (and order) matching the horizons of `x`

## Details

Horizons with `NA` depths can be flagged as overlapping. Consider
finding these horizons with `checkHzDepthLogic(byhz=TRUE)` and removing
or fixing them.

## See also

[`checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md)
[`fillHzGaps()`](https://ncss-tech.github.io/aqp/reference/fillHzGaps.md)

## Author

D.E. Beaudette, A.G. Brown

## Examples

``` r
# two overlapping horizons
z <- data.frame(
  id = 'SPC',
  top = c(0, 25, 25, 50, 75, 100, 100),
  bottom = c(25, 50, 50, 75, 100, 125, 125)
)

# init SPC
depths(z) <- id ~ top + bottom

# flag perfectly overlapping horizons
z$.overlapFlag <- flagOverlappingHz(z)

# thematic sketches
plotSPC(z, color = '.overlapFlag', hz.depths = TRUE, 
depth.axis = FALSE, cex.names = 0.85)

```
