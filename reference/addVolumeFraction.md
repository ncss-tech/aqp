# Symbolize Volume Fraction within a Soil Profile Collection Plot

Symbolize volume fraction on an existing soil profile collection plot.

## Usage

``` r
addVolumeFraction(
  x,
  colname,
  res = 10,
  cex.min = 0.1,
  cex.max = 0.5,
  pch = 1,
  col = "black"
)
```

## Arguments

- x:

  a `SoilProfileCollection` object

- colname:

  character vector of length 1, naming the column containing volume
  fraction data (horizon-level attribute). Values should be within 0-100
  percent.

- res:

  integer, resolution of the grid used to symbolize volume fraction

- cex.min:

  minimum symbol size

- cex.max:

  maximum symbol size

- pch:

  integer, plotting character code

- col:

  symbol color, either a single color or as many colors as there are
  horizons in `x`

## Details

This function can only be called after plotting a
`SoilProfileCollection` object. Details associated with a call to
[`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)
are automatically accounted for within this function: e.g. `plot.order`,
`width`, etc..

## Note

It may be necessary to adjust both `res`, `cex.min`, and `cex.max` for
optimal legibility.

## See also

[`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)

## Author

D.E. Beaudette
