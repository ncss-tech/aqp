# Visualize Color Quantiles

This function creates a visualization of the output from
`colorQuantiles` using lattice graphics.

## Usage

``` r
plotColorQuantiles(res, pt.cex = 7, lab.cex = 0.66)
```

## Arguments

- res:

  list returned by `colorQuantiles`

- pt.cex:

  scaling factor for color chips

- lab.cex:

  chip label scaling factor

## Value

a `lattice` graphics object

## Details

Marginal percentiles and L1 median CIELAB values from
[`colorQuantiles()`](https://ncss-tech.github.io/aqp/reference/colorQuantiles.md)
are combined into a single plot, arranged in panels according to L, A,
and B coordinates. Munsell "chips" (colors and labels) are based on the
closest Munsell color found via
[`col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.md).

## Author

D.E. Beaudette
