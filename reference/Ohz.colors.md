# Representative O Horizon Colors

Representative O horizon colors for the most common types of organic
soil material, at dry and moist states. Colors were derived from an
analysis of soil morphologic data (5245 horizons) within the USDA-NRCS
National Soil Information System. Representative colors are the L1
median colors (by
[`colorVariation()`](https://ncss-tech.github.io/aqp/reference/colorVariation.md))
within groups generalized to "Oi", "Oe", "Oa", and all "other". These
estimates may be useful place-holder values for soil color in
collections where O horizon color was not recorded.

## Usage

``` r
data(Ohz.colors)
```

## Format

A `data.frame` with the following elements:

- state: soil moisture state, "dry" or "moist"

- genhz: horizon designation generalized into one of "Oi", "Oe", "Oa",
  or "other"

- L1.munsell: representative color in Munsell notation for each moisture
  state / generalized horizon designation

- group.dE00: weighted-mean CIE dE 2000 color contrast metric between
  all soil colors in this group and representative color
