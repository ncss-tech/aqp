# Shannon Entropy

A very simple implementation of Shannon entropy.

## Usage

``` r
shannonEntropy(x, b = 2)
```

## Arguments

- x:

  vector of probabilities (0,1), must sum to 1, should not contain NA

- b:

  logarithm base

## Value

A single numeric value.

## Details

`0`s are automatically removed by `na.rm = TRUE`, as
`(0 * log(0) = Nan)`

## Note

When `b = length(x)` the result is the normalized Shannon entropy of
(Kempen et al, 2009).

## References

Kempen, Bas, Dick J. Brus, Gerard B.M. Heuvelink, and Jetse J.
Stoorvogel. 2009. "Updating the 1:50,000 Dutch Soil Map Using Legacy
Soil Data: A Multinominal Logistic Regression Approach." Geoderma 151:
311-26. doi:10.1016/j.geoderma.2009.04.023

Shannon, Claude E. (July-October 1948). "A Mathematical Theory of
Communication". Bell System Technical Journal. 27 (3): 379-423.
doi:10.1002/j.1538-7305.1948.tb01338.x

## Examples

``` r
# a very simple example
p <- c(0.25, 0.25, 0.4, 0.05, 0.05)

shannonEntropy(p)
#> [1] 1.960964


```
