# Transform a SPC with expressions based on site or horizon level attributes

[`transform()`](https://rdrr.io/r/base/transform.html) is a function
used for modifying columns in SoilProfileCollections.

It allows the user to specify an arbitrary number of expressions that
resolve to the (re-)calculation of one or more site or horizon level
attributes. For instance: `mutate(spc, thickness = hzdepb - hzdept)`.
The expressions may depend on one another, and are evaluated from left
to right.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
transform(`_data`, ...)
```

## Arguments

- \_data:

  A SoilProfileCollection

- ...:

  Comma-separated set of R expressions e.g.
  `thickness = hzdepb - hzdept, hzdepm = hzdept + round(thk / 2)`

## Value

A SoilProfileCollection

## Author

Andrew G. Brown.
