# Get or Set Horizon Designation Column Name

`hzdesgnname()`: Get column name containing horizon designations

`hzdesgnname<-`: Set horizon designation column name

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
hzdesgnname(object, required = FALSE)

# S4 method for class 'SoilProfileCollection'
hzdesgnname(object, required = FALSE) <- value
```

## Arguments

- object:

  a SoilProfileCollection

- required:

  logical, is this attribute required? If it is, set to `TRUE` to
  trigger error on invalid `value`.

- value:

  character, name of column containing horizon designations

## Details

Store the column name containing horizon designations or other
identifiers in the metadata slot of the SoilProfileCollection.

## See also

[`hzDesgn()`](https://ncss-tech.github.io/aqp/reference/hzDesgn.md)

## Examples

``` r
data(sp1)

# promote to SPC
depths(sp1) <- id ~ top + bottom

# set horizon designation column
hzdesgnname(sp1) <- "name"

# get horizon designation column
hzdesgnname(sp1)
#> [1] "name"
```
