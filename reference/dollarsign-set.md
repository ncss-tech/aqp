# Set data in column of horizon or site data in a SoilProfileCollection

Set the data in a column accessed by name `spc$name`. Column names other
than profile ID are not shared between site and horizons.

When using `$<-`, the length of input and output matching either the
number of sites or number of horizons is used to determine which slot
new columns are assigned to. Use `site(x)$name <- value` or
`horizons(x)$name <- value` to be explicit about which slot is being
accessed.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
x$name <- value
```

## Arguments

- x:

  a SoilProfileCollection

- name:

  a single column name in site or horizon table

- value:

  Replacement values: unit length or equal to number of horizons or
  sites.
