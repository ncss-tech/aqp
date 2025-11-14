# Generate a `SoilProfileCollection` of random profiles

This function provides a convenient abstraction of
[`lapply()`](https://rdrr.io/r/base/lapply.html),
[`random_profile()`](https://ncss-tech.github.io/aqp/reference/random_profile.md),
and
[`combine()`](https://ncss-tech.github.io/aqp/reference/combine-SoilProfileCollection-method.md)
which are typically used together to create a `SoilProfileCollection`
object with \>1 soil profiles. `rp()` creates zero-padded integer IDs
for logical sorting and indexing of profiles. For more complex IDs or
additional flexibility, see
[`random_profile()`](https://ncss-tech.github.io/aqp/reference/random_profile.md).
See
[`random_profile()`](https://ncss-tech.github.io/aqp/reference/random_profile.md)
for all possible arguments.

## Usage

``` r
rp(size, prefix = NULL, ...)
```

## Arguments

- size:

  integer, number of requested profiles

- prefix:

  prefix added to zero-padded, integer IDs

- ...:

  additional arguments to
  [`random_profile()`](https://ncss-tech.github.io/aqp/reference/random_profile.md)

## Value

a `SoilProfileCollection` object

## Examples

``` r
# generate a SoilProfileCollection object with 10 profiles
# using 0-padded, integer IDs for intuitive sorting
spc <- rp(10, method = 'LPP')
plotSPC(spc, color = 'p1')


# apply a prefix to the IDs
spc <- rp(10, prefix = 'A-', method = 'LPP')
plotSPC(spc, color = 'p1')

```
