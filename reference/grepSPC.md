# Subset SPC with pattern-matching for text-based attributes

`grepSPC()` is a shorthand function for subsetting
`SoilProfileCollection` objects. For example, by
`filter(grepl(spc, ...))` or `filter(stringr::str_detect(spc, ...))`. It
provides pattern matching for a single text-based site or horizon level
attribute.

## Usage

``` r
grepSPC(object, attr, pattern, ...)
```

## Arguments

- object:

  A SoilProfileCollection

- attr:

  A character vector (column in object) for matching patterns against.

- pattern:

  REGEX pattern to match in `attr`

- ...:

  Additional arguments are passed to
  [`grep()`](https://rdrr.io/r/base/grep.html)

## Value

A SoilProfileCollection.

## Author

Andrew G. Brown.
