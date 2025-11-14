# Get or Set Horizon Texture Class Column Name

`hztexclname()`: Get column name containing horizon designation name

`hztexclname<-`: Set horizon texture class column name for a
SoilProfileCollection

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
hztexclname(object, required = FALSE)

# S4 method for class 'SoilProfileCollection'
hztexclname(object, required = FALSE) <- value
```

## Arguments

- object:

  a SoilProfileCollection

- required:

  logical, is this attribute required? If it is, set to `TRUE` to
  trigger error on invalid `value`.

- value:

  character, name of column containing horizon texture classes

## Details

Store the column name containing horizon texture classes or other
identifiers in the metadata slot of the SoilProfileCollection.

## Examples

``` r
data(sp1)

# promote to SPC
depths(sp1) <- id ~ top + bottom

# set horizon texture class column
hztexclname(sp1) <- "texture"

# get horizon texture class column
hztexclname(sp1)
#> [1] "texture"
```
