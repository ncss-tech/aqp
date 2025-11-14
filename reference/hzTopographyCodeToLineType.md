# Convert Horizon Boundary Topography to Line Type

This function will convert USDA-NCSS horizon boundary topography codes
into line types, based on the [Field Book for Describing and Sampling
Soils, version
3.0](https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991).

## Usage

``` r
hzTopographyCodeToLineType(
  x,
  codes = c("smooth", "wavy", "irregular", "broken"),
  lty = c(1, 2, 3, 4)
)
```

## Arguments

- x:

  vector of boundary topography codes to be converted

- codes:

  character vector of topography terms ('smooth') or codes ('S'), case
  insensitive, see details

- lty:

  line types

## Value

vector of line types with same length as `x`

## Details

Visualization of horizon boundary topography can be difficult, line type
offers an additional visual cue. See `hzTopographyCodeToOffset` for an
offset-based approach. Additional examples are available in the
[Visualization of Horizon Boundaries
tutorial](https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html).
Missing data in `x` (NA) or codes that are not defined in `codes` are
returned as line type 1.

Either format (or mixture) are accepted, case insensitive:

- terms: `c('smooth', 'wavy', 'irregular', 'broken')`

- coded values: `c('s', 'w', 'i', 'b')`

## References

[Field Book for Describing and Sampling Soils, version
3.0](https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991)

## See also

[`plotSPC`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)`, `[`hzTopographyCodeToOffset`](https://ncss-tech.github.io/aqp/reference/hzTopographyCodeToOffset.md)

## Author

D.E. Beaudette
