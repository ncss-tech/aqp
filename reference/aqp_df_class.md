# Get aqp_df_class entry from metadata or return a safe value.

This is an accessor and replacement method for the `aqp_df_class` entry
in the metadata slot. This entry is used internally by methods that
interact with `data.frame` objects and slots to ensure that the same
class used to promote to the SoilProfileCollection initially is used
throughout the process.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
aqp_df_class(object)

# S4 method for class 'SoilProfileCollection'
aqp_df_class(object) <- value
```

## Arguments

- object:

  a SoilProfileCollection

- value:

  "data.frame", "data.table" or "tbl_df"
