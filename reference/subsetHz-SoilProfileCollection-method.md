# Subset the horizons in a SoilProfileCollection using logical criteria

`subsetHz()` is a function used for extracting horizons from a
SoilProfileCollection based on logical criteria.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
subsetHz(x, ..., drop = TRUE)
```

## Arguments

- x:

  a SoilProfileCollection

- ...:

  Comma-separated set of R expressions that evaluate as `TRUE` or
  `FALSE` in context of horizon data frame. Length for individual
  expressions matches number of horizons, in `x`.

- drop:

  Default: `TRUE`. When `drop=FALSE` placeholder horizons (profile ID
  with all other values `NA`) are created where the specified filter
  results in removal of all horizons.

## Value

a SoilProfileCollection with a subset of horizons, possibly with some
sites removed

## Details

To minimize likelihood of issues with non-standard evaluation context,
especially when using `subsetHz()` inside another function, all
expressions used in `...` should be in terms of variables that are in
the horizon data frame.

## Examples

``` r
data(sp3)

depths(sp3) <- id ~ top + bottom

# show just horizons with 10YR hues
plot(subsetHz(sp3, hue == '10YR'))

```
