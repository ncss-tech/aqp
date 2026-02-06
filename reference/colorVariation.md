# Quantitative Description of Color Variation

This function computes several measures of "color variation", typically
associated with soil colors described in the Munsell system, using the
CIE2000 dE (dE00) color contrast metric. The resulting dE00 summaries
map closely to color differences as perceived by "average human vision".

## Usage

``` r
colorVariation(
  m,
  method = c("frequency", "centroid", "reference", "L1"),
  ref = NULL
)
```

## Arguments

- m:

  character vector of colors, described using the Munsell system e.g.
  `c('10YR 3/3', '5YR 4/6')`

- method:

  character, one of `c('frequency', 'centroid', 'reference')`, see
  Details

- ref:

  character, a reference color specified in the Munsell system when
  `method = 'reference'`

## Value

numeric, dE00 summary of color variation along with group centroid for
`method = c('frequency', 'centroid', 'L1')`

## Details

dE00 values are computed according to `method`:

- 'frequency': relative to most frequent color in `m`

- 'centroid': relative to centroid (CIELAB coordinates) of colors
  specified in `m`

- 'L1': relative to L1-median (geometric median) CIELAB coordinates of
  colors specified in `m`, via
  [`Gmedian::Gmedian()`](https://rdrr.io/pkg/Gmedian/man/Gmedian.html)

- 'reference': relative to color specified in `ref`

The `L1` method is more robust to outliers in `m` as combined pedon
`centroid` method.

## Examples

``` r
# some brownish colors with a wild outlier
m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '5GY 6/8')

# useful when there may be a lot of duplicates
colorVariation(m, method = 'frequency')
#> [1] 10.06968
#> attr(,"most frequent")
#> [1] "10YR 4/4"

# statistical "centroid" of colors, not robust to outliers
# result may not match any color in `m`
colorVariation(m, method = 'centroid')
#> [1] 12.92885
#> attr(,"centroid")
#> [1] "5Y 4/4"

# deviation from a known reference
colorVariation(m, method = 'reference', ref = '10YR 2/1')
#> [1] 24.34378

# L1-median (requires Gmedian package) like 'centroid'
# more robust to outliers
# result will usually be very closer to a color in `m`
# colorVariation(m, method = 'L1')
```
