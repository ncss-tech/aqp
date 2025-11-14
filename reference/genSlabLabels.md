# Generate Labels for Slabs

This method is used by
[`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md) for
generating labels that assign IDs to layers in a SoilProfileCollection

## Usage

``` r
genSlabLabels(
  slab.structure = 1,
  max.d = NULL,
  n.profiles = NULL,
  spc = NULL,
  diced = NULL,
  ...
)
```

## Arguments

- slab.structure:

  A user-defined slab thickness (defined by an integer), or user-defined
  structure (numeric vector). See details for
  [`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md).

- max.d:

  Maximum depth

- n.profiles:

  Number of profiles

- spc:

  Optional: A SoilProfileCollection

- diced:

  Optional: The
  [`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md)-ed
  horizon-level data.frame corresponding to `spc`

- ...:

  Additional arguments passed to
  [`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md)
  when `spc` is specified.

## Value

factor. slab IDs, labels are the segment top and bottom depth separated
by `"-"`

## Details

The new routine used in aqp 2.0 requires that, at a minimum, the `spc`
and `slab.structure` arguments be specified.

## See also

[`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md)
