# Get the minimum bottom depth in a SoilProfileCollection

Get the shallowest depth of description out of all profiles in a
SoilProfileCollection. Data missing one or more of: bottom depth,
profile ID, or any optional attribute are omitted using
`complete.cases`.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
min(x, v = NULL, na.rm = TRUE)
```

## Arguments

- x:

  a SoilProfileCollection

- v:

  optional: a vector of horizon attribute names to refine calculation

- na.rm:

  remove `NA`? default: `TRUE`
