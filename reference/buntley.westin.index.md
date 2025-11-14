# Buntley-Westin (1965) Index

Calculate "Color Development Equivalent" by the method of Buntley &
Westin (1965) "A Comparative Study of Developmental Color in a
Chestnut-Chernozem-Brunizem Soil Climosequence" DOI:
10.2136/sssaj1965.03615995002900050029x. Originally developed for
Mollisols, the Buntley-Westin index has been used as a tool to separate
soils based on depth to particular colors.

## Usage

``` r
buntley.westin.index(hue, chroma)
```

## Arguments

- hue:

  A character vector containing Munsell hues (e.g. "7.5YR")

- chroma:

  A numeric vector containing Munsell chromas

## Value

A numeric vector reflecting horizon color development.

## References

Buntley, G.J. and Westin, F.C. (1965), A Comparative Study of
Developmental Color in a Chestnut-Chernozem-Brunizem Soil Climosequence.
Soil Science Society of America Journal, 29: 579-582.
doi:10.2136/sssaj1965.03615995002900050029x

## Author

Andrew G. Brown
