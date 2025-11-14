# Perform summaries on groups (from `group_by`) and create new site or horizon level attributes

`summarize()` is a function used for summarizing a
`SoilProfileCollection` object. Specify the groups using the group_by
verb, and then (named) expressions to evaluate on each group. The result
is a data.frame with one row per categorical level in the grouping
variable and one column for each summary variable.

## Usage

``` r
summarizeSPC(object, ...)
```

## Arguments

- object:

  A `SoilProfileCollection`

- ...:

  A set of (named) comma-delimited R expressions that resolve to a
  summary value. e.g `groupmean = mean(clay, na.rm = TRUE)`

## Value

A `data.frame` with one row per level in the grouping variable, and one
column for each summary

## Author

Andrew G. Brown
