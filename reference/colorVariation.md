# Quantitative Description of Color Variation

This function computes several measures of "color variation", typically
associated with soil colors described in the Munsell system, using the
CIE2000 dE (dE00) color contrast metric. The resulting dE00 summaries
map closely to color differences as perceived by "average human vision".

## Usage

``` r
colorVariation(m, method = c("frequency", "centroid", "reference"), ref = NULL)
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

numeric dE00 summary of color variation

## Details

dE00 values are computed according to `method`:

- 'frequency': relative to most frequency color in `m`

- 'centroid': relative to centroid (CIELAB coordinates) of colors
  specified in `m`

- 'reference': relative to color specified in `ref`

## Examples

``` r
m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '5GY 6/8')
colorVariation(m)
#> [1] 10.06968
#> attr(,"most frequent")
#> [1] "10YR 4/4"
```
