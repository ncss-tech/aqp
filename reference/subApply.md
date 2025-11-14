# Subset SPC based on result of performing function on each profile

`subApply()` is a function used for subsetting SoilProfileCollections.
It currently does NOT support for "tidy" lexical features in the `...`
arguments passed to
[`profileApply()`](https://ncss-tech.github.io/aqp/reference/profileApply.md).
The expectation is that the function `.fun` takes a single-profile
SoilProfileCollection and returns a logical value of length one. The use
case would be for any logical comparisons that cannot be evaluated
inline by `subSPC()` because they require more than simple logical
operations.

## Usage

``` r
subApply(object, .fun, ...)
```

## Arguments

- object:

  A SoilProfileCollection

- .fun, :

  A function that takes a single profile, returns *logical* of length 1.

- ...:

  Additional arguments are passed to `.fun`

## Value

A SoilProfileCollection.

## Author

Andrew G. Brown.
