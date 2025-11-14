# Convert colors into Munsell Notation

Lookup the `n` closest Munsell chips from the `munsell` lookup table
from various color notations. This function replaces
[`rgb2munsell()`](https://ncss-tech.github.io/aqp/reference/rgb2munsell.md).

## Usage

``` r
col2Munsell(col, space = c("sRGB", "CIELAB"), nClosest = 1)
```

## Arguments

- col:

  character vector of colors, `data.frame` or `matrix` of color
  coordinates in sRGB or CIELAB color space

- space:

  character, one of `sRGB` or `CIELAB`, defines the input color system

- nClosest:

  integer, number of closest Munsell colors to return (valid range is
  1-20)

## Value

an (NA-padded) `data.frame` containing `hue`, `value`, `chroma`, and CIE
delta-E 2000 color contrast metric between source and nearest matching
color(s).

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
# vector of named R colors
col2Munsell(c('red', 'green', 'blue'))
#>    hue value chroma        sigma
#> 1  10R   5.0     26 5.334215e-14
#> 2 10GY   8.5     22 1.111449e-14
#> 3  5PB   1.0     44 0.000000e+00

# sRGB matrix in the range of 0-255
col2Munsell(cbind(255, 0, 0))
#>   hue value chroma        sigma
#> 1 10R     5     26 5.334215e-14

# sRGB matrix in the range of 0-1
col2Munsell(cbind(1, 0, 0))
#>   hue value chroma        sigma
#> 1 10R     5     26 5.334215e-14

# 10YR 5/6 in CIELAB
col2Munsell(
  cbind(51.4337, 9.917916, 38.6889), 
  space = 'CIELAB'
)
#>    hue value chroma        sigma
#> 1 10YR     5      6 1.502818e-06

# 2.5YR 6/8 in hex notation
col2Munsell("#D18158FF")
#>     hue value chroma     sigma
#> 1 2.5YR     6      8 0.1117122

# 7.5YR 8/1 in sRGB {0, 1}
col2Munsell(
  cbind(0.8240707, 0.7856834, 0.7541048)
)
#>     hue value chroma     sigma
#> 1 7.5YR     8      1 0.7104182

# 7.5YR 8/1 in sRGB {0, 255}
col2Munsell(
  cbind(0.8240707, 0.7856834, 0.7541048) * 255
)
#>     hue value chroma     sigma
#> 1 7.5YR     8      1 0.7104182

# multple colors in CIELAB
col2Munsell(
  parseMunsell(c('10BG 6/6', '2.5YR 4/6'), returnLAB = TRUE),
  space = 'CIELAB'
)
#>     hue value chroma sigma
#> 1  10BG     6      6     0
#> 2 2.5YR     4      6     0

# data.frame input
col2Munsell(
  data.frame(r = 1, g = 0, b = 0),
  space = 'sRGB'
)
#>   hue value chroma        sigma
#> 1 10R     5     26 5.334215e-14

# keep examples from using more than 2 cores
data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))

# Munsell notation to sRGB triplets {0, 1} 
color <- munsell2rgb(
  the_hue = c('10YR', '2.5YR', '5YR'), 
  the_value = c(3, 5, 2.5), 
  the_chroma = c(5, 6, 2), 
  return_triplets = TRUE
)

# result is a data.frame of sRGB {0, 1}
color
#>           r         g          b
#> 1 0.3754983 0.2555129 0.09093377
#> 2 0.6613729 0.4212891 0.30856225
#> 3 0.2897926 0.2154340 0.18009210

# back-transform sRGB -> closest Munsell color
# sigma is the dE00 color contrast metric
col2Munsell(color, space = 'sRGB')
#>     hue value chroma        sigma
#> 1  10YR   3.0      5 4.410418e-14
#> 2 2.5YR   5.0      6 2.796756e-15
#> 3   5YR   2.5      2 3.177668e-14
```
