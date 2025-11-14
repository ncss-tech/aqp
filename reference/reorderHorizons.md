# Re-order corrupted horizon data

This is a method made available primarily to repair horizon data that
have been corrupted relative to their order at time of
SoilProfileCollection construction.

There is an option to specify the target order, but this will not update
the corresponding metadata entry tracking the original order. Use this
functionality at your own risk.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
reorderHorizons(object, target.order = NULL)
```

## Arguments

- object:

  A SoilProfileCollection

- target.order:

  A numeric vector of equal length to `object`. Default value is `NULL`
  which restores the internal order of the collection.

## Value

SoilProfileCollection
