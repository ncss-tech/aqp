# Check for valid spatial reference of profiles

Are coordinate column names defined in metadata and existing in the
SoilProfileCollection?

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
validSpatialData(object)
```

## Arguments

- object:

  a SoilProfileCollection

## Value

logical `TRUE` if column names are defined and correspond to existing
data
