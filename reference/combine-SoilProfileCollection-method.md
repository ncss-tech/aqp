# Combine SoilProfileCollection objects

Combine `SoilProfileCollection` objects or lists of
`SoilProfileCollection` objects. This method provides `...` expansion
for the `pbindlist` method.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
c(x, ...)

# S4 method for class 'SoilProfileCollection'
combine(...)

# S4 method for class 'list'
combine(...)
```

## Arguments

- x:

  A `SoilProfileCollection`

- ...:

  `SoilProfileCollection` objects

## Value

A SoilProfileCollection

## Examples

``` r
# example data
spc1 <- random_profile(1, SPC = TRUE)
spc2 <- random_profile(2, SPC = TRUE)
spc3 <- random_profile('A', SPC = TRUE)

# combine into a single SPC, ... interface
spc <- combine(spc1, spc2, spc3)

# combine into a single SPC, list interface
spc <- combine(list(spc1, spc2, spc3))

# input are combined into a single SPC
spc <- c(spc1, spc2, spc3)

# result is a list when a mixture of objects are provided
spc <- c(spc1, bar=spc2, baz="foo")
```
