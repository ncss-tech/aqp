# sRGB to Munsell Color Conversion

Convert sRGB color coordinates to the closest `n` Munsell chips in the
`munsell` lookup table. This function will be replaced by
[`col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.md)
in **aqp 2.1**.

## Usage

``` r
rgb2munsell(color, colorSpace = c("CIE2000", "LAB", "sRGB"), nClosest = 1)
```

## Arguments

- color:

  a `data.frame` or `matrix` object containing sRGB coordinates in the
  range of (0,1)

- colorSpace:

  distance metric (colorspace) to use for finding the closest chip:
  CIE2000 is the most accurate but requires farver \>= 2.0.3, Euclidean
  distance in CIELAB is a close second, while Euclidean distance in sRGB
  is not at all accurate and should only be used for demonstration
  purposes.

- nClosest:

  number of closest Munsell colors to return (valid range is 1-20)

## Value

an (NA-padded) `data.frame` containing `hue`, `value`, `chroma`, and
distance (dE00 when `colorSpace = 'CIE2000'`, Euclidean distance
otherwise) to nearest matching color.

## Note

This function is fully vectorized and will pad output with NA-records
when NA are present in `color`.

## References

- http://ncss-tech.github.io/AQP/

- http://www.brucelindbloom.com/index.html?ColorCalcHelp.html

- http://www.munsellcolourscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html

- http://www.cis.rit.edu/mcsl/online/munsell.php

## Author

D.E. Beaudette

## Examples

``` r
# keep examples from using more than 2 cores
data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))

# Munsell notation to sRGB triplets [0-1] 
color <- munsell2rgb(
  the_hue = c('10YR', '2.5YR', '5YR'), 
  the_value = c(3, 5, 2.5), 
  the_chroma = c(5, 6, 2), 
  return_triplets = TRUE
)

# result is a data.frame
color
#>           r         g          b
#> 1 0.3754983 0.2555129 0.09093377
#> 2 0.6613729 0.4212891 0.30856225
#> 3 0.2897926 0.2154340 0.18009210

# back-transform sRGB -> closest Munsell color
# sigma is the dE00 color contrast metric
rgb2munsell(color)
#> rgb2munsell() will be deprecated in aqp 2.1, please use col2Munsell() instead.
#>     hue value chroma        sigma
#> 1  10YR   3.0      5 4.410418e-14
#> 2 2.5YR   5.0      6 2.796756e-15
#> 3   5YR   2.5      2 3.177668e-14
```
