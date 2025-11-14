# Multinominal Brier Score

Compute a multinominal Brier score from predicted class probabilities
and observed class label. Lower values are associated with a more
accurate classifier.

## Usage

``` r
brierScore(x, classLabels, actual = "actual")
```

## Arguments

- x:

  `data.frame` of class probabilities (numeric) and observed class label
  (character), see examples

- classLabels:

  vector of predicted class labels (probabilities), corresponding to
  column names in `x`

- actual:

  name of column containing the observed class, should be character
  vector not factor

## Value

a single Brier score, representative of data in `x`

## References

Brier, Glenn W. 1950. "Verification of Forecasts Expressed in Terms of
Probability." Monthly Weather Review 78 (1): 1-3.
doi:10.1175/1520-0493(1950)078\<0001:VOFEIT\>2.0.CO;2.

## Author

D.E. Beaudette

## Examples

``` r
# columns 'a', 'b', 'c' contain predicted probabilities
# column 'actual' contains observed class label

# a good classifier
d.good <- data.frame(
  a = c(0.05, 0.05, 0.10),
  b = c(0.90, 0.85, 0.75),
  c = c(0.05, 0.10, 0.15),
  actual = c('b', 'b', 'b'),
  stringsAsFactors = FALSE
)

# a rather bad classifier
d.bad <- data.frame(
  a = c(0.05, 0.05, 0.10),
  b = c(0.90, 0.85, 0.75),
  c = c(0.05, 0.10, 0.15),
  actual = c('c', 'c', 'c'),
  stringsAsFactors = FALSE
)

# class labels are factors
d.factors <- data.frame(
  a = c(0.05, 0.05, 0.10),
  b = c(0.90, 0.85, 0.75),
  c = c(0.05, 0.10, 0.15),
  actual = c('b', 'b', 'b'),
  stringsAsFactors = TRUE
)

# relatively low value = accurate
brierScore(x = d.good, classLabels = c('a', 'b', 'c'), actual = 'actual')
#> [1] 0.04833333

# high values = not accuate
brierScore(x = d.bad, classLabels = c('a', 'b', 'c'), actual = 'actual')
#> [1] 1.515

# message related to conversion of factor -> character
brierScore(x = d.factors, classLabels = c('a', 'b', 'c'), actual = 'actual')
#> converting `actual` from factor to character
#> [1] 0.04833333
```
