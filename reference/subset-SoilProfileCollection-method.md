# Subset a SoilProfileCollection with logical expressions

`subset()` is a function used for extracting profiles from a
SoilProfileCollection based on logical criteria. It allows the user to
specify an arbitrary number of logical vectors (equal in length to site
or horizon), separated by commas. The function includes some support for
non-standard evaluation.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
subset(x, ..., greedy = FALSE)
```

## Arguments

- x:

  A SoilProfileCollection

- ...:

  Comma-separated set of R expressions that evaluate as TRUE or FALSE.
  Length for individual expressions matches number of sites OR number of
  horizons, in `object`.

- greedy:

  Use "greedy" matching for combination of site and horizon level
  matches? `greedy = TRUE` is the union, whereas `greedy = FALSE`
  (default) is the intersection (of site and horizon matches).

## Value

A SoilProfileCollection.

## Details

To minimize likelihood of issues with non-standard evaluation context,
especially when using `subset()` inside another function, all
expressions used in `...` should be in terms of variables that are in
the site or horizon data frame.

## Author

Andrew G. Brown.
