# Find horizons with colors darker than a Munsell hue, value, chroma threshold

`hasDarkColors` returns a boolean value by horizon representing whether
darkness thresholds are met. The code is fully vectorized and deals with
missing data and optional thresholds.

Default arguments are set up for "5-3-3 colors" – the basic criteria for
Mollic/Umbric epipedon/mineral soil darkness. Any of the thresholds or
column names can be altered. Any thresholds that are set equal to `NA`
will be ignored.

## Usage

``` r
hasDarkColors(
  p,
  d_hue = NA,
  m_hue = NA,
  d_value = 5,
  d_chroma = NA,
  m_value = 3,
  m_chroma = 3,
  dhuenm = "d_hue",
  dvalnm = "d_value",
  dchrnm = "d_chroma",
  mhuenm = "m_hue",
  mvalnm = "m_value",
  mchrnm = "m_chroma"
)
```

## Arguments

- p:

  A SoilProfileCollection.

- d_hue:

  Optional: character vector of dry hues to match (default: NA)

- m_hue:

  Optional: character vector of moist hues to match (default: NA)

- d_value:

  Maximum value of dry value (default: 5)

- d_chroma:

  Optional: Maximum value of dry chroma (default: NA)

- m_value:

  Maximum value of moist value (default: 3)

- m_chroma:

  Maximum value of moist chroma (default: 3)

- dhuenm:

  Column name containing dry hue.

- dvalnm:

  Column name containing dry value.

- dchrnm:

  Column name containing dry chroma.

- mhuenm:

  Column name containing moist hue.

- mvalnm:

  Column name containing moist value.

- mchrnm:

  Column name containing moist chroma.

## Value

Boolean value (for each horizon in `p`) reflecting whether "darkness"
criteria are met.

## Author

Andrew G. Brown

## Examples

``` r
# construct a fake profile
spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxeralfs",
                  hzdesgn  = c("A","AB","Bt","BCt","R"),
                  hzdept   = c(0, 20, 32, 42,  49),
                  hzdepb   = c(20, 32, 42, 49, 200),
                  d_value  = c(5,   5,  5,  6,  NA),
                  m_value  = c(2.5, 3,  3,  4,  NA),
                  m_chroma = c(2,   3,  4,  4,  NA))

# promote to SoilProfileCollection
depths(spc) <- id ~ hzdept + hzdepb

# print results in table
data.frame(id = spc[[idname(spc)]],
           hz_desgn = spc$hzdesgn,
           has_dark_colors = hasDarkColors(spc))
#>   id hz_desgn has_dark_colors
#> 1  1        A            TRUE
#> 2  1       AB            TRUE
#> 3  1       Bt           FALSE
#> 4  1      BCt           FALSE
#> 5  1        R              NA
```
