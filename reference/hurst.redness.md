# Hurst (1977) Redness Index

Calculate Redness Index after Hurst (1977) "Visual estimation of iron in
saprolite" DOI: 10.1130/0016-7606(1977)88\<174:VEOIIS\>2.0.CO;2. Accepts
vectorized inputs for hue, value and chroma, produces vector output.

## Usage

``` r
hurst.redness(hue, value, chroma)
```

## Arguments

- hue:

  A character vector containing Munsell hues (e.g. "7.5YR")

- value:

  A numeric vector containing Munsell values

- chroma:

  A numeric vector containing Munsell chromas

## Value

A numeric vector of horizon redness index (lower values = redder).

## References

Hurst, V.J. (1977) Visual estimation of iron in saprolite. GSA Bulletin.
88(2): 174–176. doi:
https://doi.org/10.1130/0016-7606(1977)88\<174:VEOIIS\>2.0.CO;2

## Author

Andrew G. Brown
