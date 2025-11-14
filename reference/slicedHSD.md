# Tukey's HSD Over Slices

Apply Tukey's HSD over 1-unit depth slices.

## Usage

``` r
slicedHSD(object, fm, conf = 0.95)
```

## Arguments

- object:

  `SoilProfileCollection` object

- fm:

  a formula describing depth sequence, horizon attribute, and site
  (grouping) attribute. For example 0:100 ~ estimated_oc \| taxonname

- conf:

  confidence applied in `TukeyHSD`

## Author

D.E. Beaudette and Sharon Perrone
