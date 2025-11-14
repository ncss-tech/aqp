# Add or change column of horizon or site data in a SoilProfileCollection

Add or change the data from a column accessed by name. Column names
other than profile ID are not shared between site and horizons. The
benefit of using double bracket setter over `$` is that `name` can be
calculated, whereas with `$`, it must be known a priori and hard coded.

When using the double bracket setter the length of input and output
matching either the number of sites or number of horizons is used to
determine which slot new columns are assigned to.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
x[[i]] <- value
```

## Arguments

- x:

  a SoilProfileCollection

- i:

  an expression resolving to a single column name in site or horizon
  table-

- value:

  New value to replace – unit length or equal in length to number of
  sites or horizons in the collection.
