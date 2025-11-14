# Get depth units from metadata

Get units of depth measurement from metadata. Default value is
centimeters.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
depth_units(object)

# S4 method for class 'SoilProfileCollection'
depth_units(object) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- value:

  character, a value representing units. Default `'cm'`.

## Examples

``` r
data(sp5)

## get depth units
du <- depth_units(sp5)

# set alternate units; e.g. inches
depth_units(sp5) <- 'in'

# replace original value (cm)
depth_units(sp5) <- du
```
