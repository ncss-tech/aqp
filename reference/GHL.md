# Get or Set Generalized Horizon Label (GHL) Column Name

`GHL()`: Get column name containing generalized horizon labels

`GHL<-`: Set generalized horizon label column name

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
GHL(object, required = FALSE)

# S4 method for class 'SoilProfileCollection'
GHL(object, required = FALSE) <- value
```

## Arguments

- object:

  a SoilProfileCollection

- required:

  logical, is this attribute required? If it is, set to `TRUE` to
  trigger error on invalid `value`.

- value:

  character, name of column containing generalized horizon labels

## Details

Store the column name containing generalized horizon labels in the
metadata slot of the SoilProfileCollection.

## Examples

``` r
data(sp1)

# promote to SPC
depths(sp1) <- id ~ top + bottom

# set horizon designation column
GHL(sp1) <- "name"

# get horizon designation column
GHL(sp1)
#> [1] "name"
```
