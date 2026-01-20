# Get names of columns in site and horizons table

Get names of columns in site and horizons table of a
SoilProfileCollection.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
names(x)

# S3 method for class 'SoilProfileCollection'
.DollarNames(x, pattern)
```

## Arguments

- x:

  a SoilProfileCollection

- pattern:

  A regular expression. Only matching names are returned.

## Details

The `.DollarNames` SoilProfileCollection method is implemented to
support auto-completion after the `$` operator. There is a custom help
handler implemented for interactive use in RStudio to integrate help
text and data viewer functionality with auto-completion. This custom
help handler can be disabled by setting
`options(.aqp.rs.rpc.integration=FALSE)`.
