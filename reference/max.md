# Get the maximum bottom depth in a SoilProfileCollection

Get the deepest depth of description out of all profiles in a
SoilProfileCollection. Data missing one or more of: bottom depth,
profile ID, or any optional attribute are omitted using
`complete.cases`.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
max(x, v = NULL, na.rm = TRUE)
```

## Arguments

- x:

  a SoilProfileCollection

- v:

  optional: horizon-level column name to refine calculation

- na.rm:

  remove `NA`? default: `TRUE`
