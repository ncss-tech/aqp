# Munsell to sRGB Lookup Table for Common Soil Colors

A lookup table of interpolated Munsell color chips for common soil
colors.

## Usage

``` r
data(munsell)
```

## Format

A data.frame with 8825 rows.

- `hue`: Munsell Hue, upper case

- `value`: Munsell Value

- `chroma`: Munsell Chroma

- `r`: sRGB "red" value (0-1)

- `g`: sRGB "green" value (0-1)

- `b`: sRGB "blue" value (0-1)

- `L`: CIELAB "L" coordinate

- `A`: CIELAB "A" coordinate

- `B`: CIELAB "B" coordinate

## Source

Color chip XYZ values:
<https://www.rit.edu/science/munsell-color-science-lab-educational-resources#munsell-renotation-data>

## Details

See `munsell2rgb` for conversion examples. Values are referenced to the
D65 standard illuminant.

## References

- Color conversion equations

  - http://www.brucelindbloom.com/index.html?ColorCalcHelp.html

- Methods used to generate this table

  - http://dx.doi.org/10.1016/j.cageo.2012.10.020

## Examples

``` r
data(munsell)
```
