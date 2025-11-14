# Retrieve metadata from SoilProfileCollection

Get metadata from SoilProfileCollection. Result is a list. Two entries
(aqp_df_class, depth_units) should not be edited in the metadata list
directly. There are methods that facilitate changing them – and
propagating their changes throughout the collection. Otherwise, metadata
list is a free-form slot used to store arbitrary information about the
data, how it was collected, citations, etc.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
metadata(object)

# S4 method for class 'SoilProfileCollection'
metadata(object) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- value:

  A named list (see examples)

## Examples

``` r
data(sp5)

# replace default metadata with itself
metadata(sp5) <- metadata(sp5)

# set new metadata attribute value
metadata(sp5)$newvalue <- 'foo'

# get metadata attribute
metadata(sp5)$newvalue
#> [1] "foo"
```
