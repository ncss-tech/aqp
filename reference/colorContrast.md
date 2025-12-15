# Metrics of Contrast Suitable for Comparing Soil Colors

Pair-wise comparisons of Munsell color specifications, based on the NCSS
color contrast classes (Soil Survey Technical Note 2) and CIE delta-E
2000 metric.

## Usage

``` r
colorContrast(m1, m2 = NULL)
```

## Arguments

- m1:

  vector of Munsell colors ('10YR 3/3'), must be \>1 color if `m2` is
  not specified

- m2:

  optional vector of Munsell colors ('10YR 3/6')

## Value

`data.frame` with the following columns:

- m1: Munsell color 1

- m2: Munsell color 2

- dH: delta-hue, as computed by `huePosition`

- dV: delta-value, absolute value of difference in Munsell value (m1 vs.
  m2)

- dc: delta-chroma, absolute value of difference in Munsell chroma (m1
  vs. m2)

- dE00: delta-E00, e.g. the [CIE delta-E as refined in
  2000](https://en.wikipedia.org/wiki/Color_difference#CIEDE2000)

- cc: soil color contrast class, as specified in Soil Survey Technical
  Note 2.

## Details

This function is fully vectorized over elements of `m1` and `m2` when
both are of the same length. Recycling is not implemented. The minimum
set of all pair-wise comparisons are performed within elements of `m1`
when `m2` is not specified. For example, comparisons between A, B, and C
are limited to *A*x*B*, *A*x*C*, and *B*x*C*. Use
[`expand.grid()`](https://rdrr.io/r/base/expand.grid.html) to generate
suitable input from 1:many or many:1 type comparisons. See [this
tutorial](http://ncss-tech.github.io/AQP/aqp/color-contrast.md) for an
expanded discussion and more examples. Neutral colors are not mentioned
in SSTN2: in this function any comparison to a neutral color (e.g. 'N
3/') are assigned a delta-hue of 1. Since SSTN2 expects hues to be
counted clock wise from 5R, it possible to get very large delta-hue
values for otherwise adjacent colors: '5R' vs. '2.5R'. This will be
addressed in an update to the standards.

The most meaningful representation of color contrast is the CIE2000
(dE00) metric.

## Note

delta-E00 is computed by the [farver
package](https://CRAN.R-project.org/package=farver).

## References

1.  https://en.wikipedia.org/wiki/Color_difference

## See also

[colorContrastPlot](https://ncss-tech.github.io/aqp/reference/colorContrastPlot.md),
[huePosition](https://ncss-tech.github.io/aqp/reference/huePosition.md),
[huePositionCircle](https://ncss-tech.github.io/aqp/reference/huePositionCircle.md)

## Author

D.E. Beaudette

## Examples

``` r
# keep examples from using more than 2 cores
data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))

# two sets of colors to compare
m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', '7.5YR 3/4')
m2 <- c('5YR 3/4', '7.5YR 4/4', '2.5YR 2/2', '7.5YR 6/3')

# contrast metrics
colorContrast(m1, m2)
#>          m1        m2 dH dV dC      dE00        cc
#> 1  10YR 6/3   5YR 3/4  2  3  1 31.286374 Prominent
#> 2 7.5YR 3/3 7.5YR 4/4  0  1  1  9.657423     Faint
#> 3  10YR 2/2 2.5YR 2/2  3  0  0  6.814101     Faint
#> 4 7.5YR 3/4 7.5YR 6/3  0  3  1 30.261643  Distinct

# adjacent chips
colorContrast('10YR 3/3', '10YR 3/4')
#>         m1       m2 dH dV dC     dE00    cc
#> 1 10YR 3/3 10YR 3/4  0  0  1 3.151168 Faint
colorContrast('10YR 3/3', '7.5YR 3/3')
#>         m1        m2 dH dV dC     dE00    cc
#> 1 10YR 3/3 7.5YR 3/3  1  0  0 3.313277 Faint

# highly contrasting colors
# http://colour.granjow.net/fabercastell-polychromos.html
colorContrastPlot('10B 4/13', '10YR 10/15',
labels = c('helioblue-reddish', 'light cadmium yellow')
)



## Note: neutral hues aren't defined in TN2
# approximation / extension of the concept
colorContrast(m1 = 'N 3/', m2 = 'N 6/')
#>     m1   m2 dH dV dC     dE00       cc
#> 1 N 3/ N 6/  0  3  0 27.24374 Distinct

colorContrast(m1 = '10YR 3/3', m2 = 'N 3/')
#>         m1   m2 dH dV dC     dE00        cc
#> 1 10YR 3/3 N 3/  1  0  3 13.63413 Prominent

# pair-wise comparisons
m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', 'N 3/')
colorContrast(m1)
#>          m1        m2 dH dV dC      dE00        cc
#> 1  10YR 6/3 7.5YR 3/3  1  3  0 30.517796 Prominent
#> 2  10YR 6/3  10YR 2/2  0  4  1 37.558326 Prominent
#> 3  10YR 6/3      N 3/  1  3  3 32.761012 Prominent
#> 4 7.5YR 3/3  10YR 2/2  1  1  1  9.252527     Faint
#> 5 7.5YR 3/3      N 3/  1  0  3 14.016566 Prominent
#> 6  10YR 2/2      N 3/  1  1  2 12.631128     Faint
```
