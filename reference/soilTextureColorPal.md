# Soil Texture Color Palettes

Suggested color palettes for USDA soil texture classes, ranked according
to average plant-available water holding capacity. The default color
mapping schema is based on a palette used by SoilWeb applications.

## Usage

``` r
soilTextureColorPal(simplify = FALSE, schema = "soilweb")
```

## Arguments

- simplify:

  logical, return the base 12 (`TRUE`) or full 21 (`FALSE`) soil texture
  classes

- schema:

  select mapping between soil texture classes, and colors, currently
  limited to 'soilweb'

## Value

`data.frame` from soil texture class codes and colors

## Author

D.E. Beaudette, Mike Walkinshaw, A.T. O'Geen

## Examples

``` r
 
# base 12 soil texture classes
# ranked by plant available water-holding capacity
d <- soilTextureColorPal(simplify = TRUE)
soilPalette(d$color, lab = d$class, lab.cex = 1)


# full 21 soil texture classes
# ranked by plant available water-holding capacity
d <- soilTextureColorPal(simplify = FALSE)
soilPalette(d$color, lab = d$class, lab.cex = 1)

```
