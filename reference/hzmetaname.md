# Get or Set Horizon Metadata Column Name

`hzmetaname()`: Get column name containing horizon data of interest

`hzmetaname<-`: Set horizon designation column name

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
hzmetaname(object, attr, required = FALSE)

# S4 method for class 'SoilProfileCollection'
hzmetaname(object, attr, required = FALSE) <- value
```

## Arguments

- object:

  A *SoilProfileCollection*

- attr:

  *character*. Base name for attribute to be stored in metadata. This is
  prefixed with `"aqp_hz"` for horizon-level metadata for column
  attributes. e.g. `attr="clay"` results in metadata value retrieved
  from `"aqp_hzclay"`.

- required:

  *logical*. Is this attribute required? If it is, set to `TRUE` to
  trigger error on invalid `value`.

- value:

  *character*. Name of horizon-level column containing data
  corresponding to `attr`.

## Details

Store the column name containing a specific type of horizon data in the
metadata slot of the SoilProfileCollection.

## See also

[`guessHzAttrName()`](https://ncss-tech.github.io/aqp/reference/guessHzAttrName.md)

## Examples

``` r
data(sp1)

# promote to SPC
depths(sp1) <- id ~ top + bottom

# set important metadata columns
hzdesgnname(sp1) <- "name"
hztexclname(sp1) <- "texture"

# set custom horizon property (clay content) column
hzmetaname(sp1, "clay") <- "prop"

# inspect metadata list
metadata(sp1)
#> $aqp_df_class
#> [1] "data.frame"
#> 
#> $aqp_group_by
#> [1] ""
#> 
#> $aqp_hzdesgn
#> [1] "name"
#> 
#> $aqp_hztexcl
#> [1] "texture"
#> 
#> $depth_units
#> [1] "cm"
#> 
#> $stringsAsFactors
#> [1] FALSE
#> 
#> $aqp_hzclay
#> [1] "prop"
#> 

# get horizon clay content column
hzmetaname(sp1, "clay")
#> [1] "prop"

# uses hzdesgname(), hztexclname(), hzmetaname(attr="clay") in function definition
estimatePSCS(sp1)
#>     id pscs_top pscs_bottom
#> 1 P001       49          89
#> 2 P002       30          59
#> 3 P003        2          52
#> 4 P004       32          62
#> 5 P005        5          55
#> 6 P006       31         106
#> 7 P007       25         100
#> 8 P008       27         102
#> 9 P009       28         103
```
