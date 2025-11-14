# Get data from column of horizon or site data in a SoilProfileCollection

Get the data from a column accessed by name `x$name`. Column names other
than profile ID are not shared between site and horizons.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
x$name
```

## Arguments

- x:

  a SoilProfileCollection

- name:

  a single column name in site or horizon table

## Examples

``` r
data(sp1)

depths(sp1) <- id ~ top + bottom

# get data from a column by name (prop)
sp1$prop
#>  [1] 13  7  9 14 21 NA  1  3  2  4 14 18 22 30 50  5  6  6 25 57  8 14 23 57  0
#> [26]  2  5  7  5  5  6 10 15 12  3  3  3  3  5  0  0  0  0  0  1  2  2  4  7  6
#> [51]  1  0  5  3  3  3  2 27  3  5
```
