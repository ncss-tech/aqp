# Convert Horizon Boundary Topography to Vertical Offset

This function will convert USDA-NCSS horizon boundary topography codes
into a vertical offset, suitable for use in `plotSPC`. Default values
are reasonable starting points for encoding smooth, wavy, irregular, or
broken style horizon boundary topography as defined in [Field Book for
Describing and Sampling Soils, version
3.0](https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991).

## Usage

``` r
hzTopographyCodeToOffset(
  x,
  codes = c("smooth", "wavy", "irregular", "broken"),
  offset = c(0, 4, 8, 12)
)
```

## Arguments

- x:

  vector of boundary topography codes to be converted

- codes:

  character vector of topography terms ('smooth') or codes ('S'), case
  insensitive, see details

- offset:

  vertical offset (depth units) used to create "chevron" effect

## Value

vector of vertical offsets with same length as `x`

## Details

Additional examples are available in the [Visualization of Horizon
Boundaries
tutorial](https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html).
Missing data in `x` (NA) or codes that are not defined in `codes` are
returned with an offset of 0.

Either format (or mixture) are accepted, case insensitive:

- terms: `c('smooth', 'wavy', 'irregular', 'broken')`

- coded values: `c('s', 'w', 'i', 'b')`

## References

[Field Book for Describing and Sampling Soils, version
3.0](https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991)

## See also

[`plotSPC`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)

## Author

D.E. Beaudette
