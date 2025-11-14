# Barron & Torrent (1986) Redness Index in LAB color space

Calculate Redness Index after Barron & Torrent (1986) "Use of the
Kubelka—Munk Theory to Study the Influence of Iron Oxides on Soil
Colour" using Munsell colors converted to LAB. DOI:
10.1111/j.1365-2389.1986.tb00382.x. Accepts vectorized inputs for hue,
value and chroma, produces vector output.

## Usage

``` r
barron.torrent.redness.LAB(hue, value, chroma)
```

## Arguments

- hue:

  A character vector containing Munsell hues (e.g. "7.5YR")

- value:

  A numeric vector containing Munsell values

- chroma:

  A numeric vector containing Munsell chromas

## Value

A numeric vector of horizon redness index (higher values = redder).

## References

Barron, V. and Torrent, J. (1986), Use of the Kubelka-Munk theory to
study the influence of iron oxides on soil colour. Journal of Soil
Science, 37: 499-510. doi:10.1111/j.1365-2389.1986.tb00382.x

## Author

Andrew G. Brown
