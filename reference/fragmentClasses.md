# Coarse Fragment Class Labels and Diameter

This is a convenience function for accessing coarse fragment class
labels and associated diameter (mm), as defined in various
classification systems such as USDA, Unified, and AASHTO.

## Usage

``` r
fragmentClasses(
  sys = c("usda_simplified", "usda", "international", "unified", "aashto",
    "mod.wentworth"),
  flat = FALSE,
  rounded = FALSE
)
```

## Arguments

- sys:

  character, length 1. This is an abbreviated name used to select class
  labels and fragment diameter.

- flat:

  logical. Fragments are flat, only used by USDA systems.

- rounded:

  logical. Fragments are rounded, only used by AASHTO system.

## Value

named vector of fragment diameter in mm

## References

Schoeneberger, P.J., D.A. Wysocki, E.C. Benham, and Soil Survey Staff.
2012. Field book for describing and sampling soils, Version 3.0. Natural
Resources Conservation Service, National Soil Survey Center, Lincoln,
NE.

## See also

[`fragmentSieve()`](https://ncss-tech.github.io/aqp/reference/fragmentSieve.md)

## Examples

``` r
# use default system: "usda_simplified"
fragmentClasses()
#>   gravel  cobbles   stones boulders 
#>       76      250      600   100000 
fragmentClasses(flat = TRUE)
#>   channers flagstones     stones   boulders 
#>        150        380        600     100000 

fragmentClasses(sys = 'usda')
#>   fine_gravel medium_gravel coarse_gravel       cobbles        stones 
#>             5            20            76           250           600 
#>      boulders 
#>        100000 
fragmentClasses(sys = 'USDA', flat = TRUE)
#>   channers flagstones     stones   boulders 
#>        150        380        600     100000 

fragmentClasses(sys = 'international')
#> gravel stones 
#>  2e+01  1e+05 

fragmentClasses(sys = 'unified')
#>   fine_gravel coarse_gravel       cobbles      boulders 
#>            19            76           300        100000 

fragmentClasses(sys = 'aashto')
#>   fine_gravel medium_gravel coarse_gravel   broken_rock 
#>       9.5e+00       2.5e+01       7.5e+01       1.0e+05 
fragmentClasses(sys = 'aashto', rounded = TRUE)
#>   fine_gravel medium_gravel coarse_gravel      boulders 
#>       9.5e+00       2.5e+01       7.5e+01       1.0e+05 

fragmentClasses(sys = 'mod.wentworth')
#>  pebbles  cobbles boulders 
#>       64      256   100000 
```
