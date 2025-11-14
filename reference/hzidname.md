# Set horizon ID column name

Set unique horizon ID column name

Get column name containing unique horizon ID

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
hzidname(object) <- value

# S4 method for class 'SoilProfileCollection'
hzidname(object)
```

## Arguments

- object:

  a SoilProfileCollection

- value:

  character, column name containing unique horizon ID values

## Examples

``` r
data(sp1)

# promote to SPC
depths(sp1) <- id ~ top + bottom

# create new horizon ID
sp1$hzIDrev <- rev(sp1$hzID)

# set horizon designation column
hzidname(sp1) <- "hzIDrev"

# get horizon designation column
hzidname(sp1)
#> [1] "hzIDrev"
```
