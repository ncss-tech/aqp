# Estimate available water capacity for fine-earth fraction

Estimate available water capacity for fine-earth fraction

## Usage

``` r
estimateAWC(texcl, omcl, precision = 2, FUN = mean, ...)
```

## Arguments

- texcl:

  character, USDA textural class fine earth fraction

- omcl:

  integer, Organic matter class. 1: less than 1.5 percent, 2: less than
  5, 3: greate than 5

- precision:

  integer, Number of decimal places in result default: 2

- FUN:

  Function for interpolating between table values default: `mean`

- ...:

  Additional arguments to `FUN`

## Value

A numeric vector double containing estimated available water capacities
for fine-earth fraction.

## Examples

``` r
# organic matter, loam texture, low medium and high OM
base.awc <- estimateAWC(c("l","l","l"), c(1, 2, 3), na.rm = TRUE)
base.awc
#> [1] 0.17 0.18 0.21
```
