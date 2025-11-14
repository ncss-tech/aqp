# SoilProfileCollection show method

Pretty output method for SoilProfileCollection objects. By default this
method limits output to 10 columns and 6 rows from the site and horizon
tables respectively.

There is an aqp environment option you can set to increase the number of
columns shown by default: `options(.aqp.show.n.cols = 100)`,

[`as.character()`](https://rdrr.io/r/base/character.html): Character
Representation of SoilProfileCollection Object

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
show(object)

# S4 method for class 'SoilProfileCollection'
as.character(x, ...)
```

## Arguments

- object:

  a SoilProfileCollection

- x:

  a SoilProfileCollection

- ...:

  additional arguments (not used)

## Examples

``` r
# load a SoilProfileCollection
data(sp5)

# use the show() method
show(sp5)
#> SoilProfileCollection with 296 profiles and 1539 horizons
#> profile ID: soil  |  horizon ID: hzID 
#> Depth range: 70 - 841 cm
#> 
#> ----- Horizons (6 / 1539 rows  |  10 / 19 columns) -----
#>    soil hzID top bottom name sand silt clay  R25  G25
#>   soil1    1   0      8   H1 32.3 10.9 52.8 0.41 0.38
#>   soil1    2   8     25   H2 29.0 11.2 58.2 0.31 0.28
#>   soil1    3  25     55   H3 34.9 11.6 51.9 0.31 0.28
#>   soil1    4  55    100   H4 38.2 10.9 49.7 0.31 0.28
#>   soil1    5 100    267   H5 32.2 10.8 55.6 0.55 0.46
#>  soil10    6   0     10   H1 25.2 14.4 58.4 0.43 0.37
#> [... more horizons ...]
#> 
#> ----- Sites (6 / 296 rows  |  1 / 1 columns) -----
#>     soil
#>    soil1
#>   soil10
#>  soil100
#>  soil101
#>  soil102
#>  soil103
#> [... more sites ...]
#> 
#> Spatial Data:
#> [EMPTY]

# which is same as this (in the console)
sp5
#> SoilProfileCollection with 296 profiles and 1539 horizons
#> profile ID: soil  |  horizon ID: hzID 
#> Depth range: 70 - 841 cm
#> 
#> ----- Horizons (6 / 1539 rows  |  10 / 19 columns) -----
#>    soil hzID top bottom name sand silt clay  R25  G25
#>   soil1    1   0      8   H1 32.3 10.9 52.8 0.41 0.38
#>   soil1    2   8     25   H2 29.0 11.2 58.2 0.31 0.28
#>   soil1    3  25     55   H3 34.9 11.6 51.9 0.31 0.28
#>   soil1    4  55    100   H4 38.2 10.9 49.7 0.31 0.28
#>   soil1    5 100    267   H5 32.2 10.8 55.6 0.55 0.46
#>  soil10    6   0     10   H1 25.2 14.4 58.4 0.43 0.37
#> [... more horizons ...]
#> 
#> ----- Sites (6 / 296 rows  |  1 / 1 columns) -----
#>     soil
#>    soil1
#>   soil10
#>  soil100
#>  soil101
#>  soil102
#>  soil103
#> [... more sites ...]
#> 
#> Spatial Data:
#> [EMPTY]
```
