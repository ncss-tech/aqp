# Compute weighted naive and *tau* statistics for a cross-classification matrix

`tauW`: Computes: (1) unweighted naive, (2) weighted naive, (3)
unweighted *tau*, (4) weighted *tau* accuracy statistics

## Usage

``` r
tauW(
  CM,
  W = diag(sqrt(length(as.matrix(CM)))),
  P = rep(1/nrow(as.matrix(CM)), nrow(as.matrix(CM)))
)

summaryTauW(result.tau)
```

## Arguments

- CM:

  a square confusion (cross-classification) matrix (rows: allocation,
  columns: reference)

- W:

  weights: 1 on diagonals, \[-1..1\] off giving partial credit to this
  error

- P:

  prior probability vector, length = number of rows/columns in `CM` and
  `W`

- result.tau:

  `tauW()` result

## Value

Results are returned in a list with obvious R names

## Details

`summaryTauW`: prints a summary of the results from *tauW*

`xtableTauW`: formats a LaTeX table with results from *tauW* and saves
it as a `.tex` file for import into a LaTeX document.

Input matrices `CM` and `W` may be in `data.frame` format and will be
converted

Weights matrix `W`: 0 = no credit; 1 = full credit; -1 = maximum penalty

If absent, default is no partial credit, i.e., unweighted.

Prior probabilities vector `P`: If absent, `P` are equal priors for each
class. Special value `P = 0` is interpreted as `P` = column marginals.

Error checks: `CM` must be square; `P` must have correct number of
classes and sum to 1 +/- 0.0001; `W` & `CM` must be conformable

## References

- Rossiter, D. G., Zeng, R., & Zhang, G.-L. (2017). Accounting for
  taxonomic distance in accuracy assessment of soil class predictions.
  Geoderma, 292, 118–127.
  [doi:10.1016/j.geoderma.2017.01.012](https://doi.org/10.1016/j.geoderma.2017.01.012)

- Ma, Z. K., & Redmond, R. L. (1995). Tau-coefficients for accuracy
  assessment of classification of remote-sensing data. Photogrammetric
  Engineering and Remote Sensing, 61(4), 435–439.

- Naesset, E. (1996). Conditional tau coefficient for assessment of
  producer’s accuracy of classified remotely sensed data. ISPRS Journal
  of Photogrammetry and Remote Sensing, 51(2), 91–98.
  [doi:10.1016/0924-2716(69)00007-4](https://doi.org/10.1016/0924-2716%2869%2900007-4)

## Author

D.G. Rossiter

## Examples

``` r
# example confusion matrix
# rows: allocation (user's counts)
# columns: reference (producer's counts)
crossclass <- matrix(data=c(2,1,0,5,0,0,
                            1,74,2,1,3,6,
                            0,5,8,6,1,3,
                            6,1,3,91,0,0,
                            0,4,0,0,0,4,
                            0,6,2,2,4,38),
                     nrow=6, byrow=TRUE)
row.names(crossclass) <- c("OP", "SA", "UA", "UC", "AV", "AC")
colnames(crossclass) <- row.names(crossclass)

# build the weights matrix
# how much credit for a mis-allocation
weights <- matrix(data=c(1.00,0.05,0.05,0.15,0.05,0.15,
                         0.05,1.00,0.05,0.05,0.05,0.35,
                         0.05,0.05,1.00,0.20,0.15,0.15,
                         0.15,0.05,0.25,1.00,0.10,0.25,
                         0.05,0.10,0.15,0.10,1.00,0.15,
                         0.20,0.30,0.10,0.25,0.20,1.00),
                  nrow=6, byrow=TRUE)

# unweighted accuracy
summaryTauW(nnaive <- tauW(crossclass))
#> [1] Cross-classification matrix:
#>      OP SA UA UC AV AC
#> [1,]  2  1  0  5  0  0
#> [2,]  1 74  2  1  3  6
#> [3,]  0  5  8  6  1  3
#> [4,]  6  1  3 91  0  0
#> [5,]  0  4  0  0  0  4
#> [6,]  0  6  2  2  4 38
#> [1] Number of observations: 279
#> [1] Weights:
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1    0    0    0    0    0
#> [2,]    0    1    0    0    0    0
#> [3,]    0    0    1    0    0    0
#> [4,]    0    0    0    1    0    0
#> [5,]    0    0    0    0    1    0
#> [6,]    0    0    0    0    0    1
#> [1] Overall accuracy (unweighted): 0.7634
#> [1] Overall accuracy (weighted): 0.7634
#> [1] User's accuracy (unweighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2500 0.8506 0.3478 0.9010 0.0000 0.7308 
#> [1] User's accuracy (weighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2500 0.8506 0.3478 0.9010 0.0000 0.7308 
#> [1] Producer's reliability (unweighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2222 0.8132 0.5333 0.8667 0.0000 0.7451 
#> [1] Producer's reliability (weighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2222 0.8132 0.5333 0.8667 0.0000 0.7451 
#> [1] Reference class proportions:
#>     OP     SA     UA     UC     AV     AC 
#> 0.0323 0.3262 0.0538 0.3763 0.0287 0.1828 
#> [1] Observed class proportions:
#>     OP     SA     UA     UC     AV     AC 
#> 0.0287 0.3118 0.0824 0.3620 0.0287 0.1864 
#> [1] Prior class probabilities:
#> [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
#> [1] Tau (unweighted): 0.7161
#> [1] Tau (weighted): 0.7161

# unweighted tau with equal priors, equivalent to Foody (1992) modified Kappa
tauW(crossclass)$tau
#> [1] 0.716129

# unweighted tau with user's = producer's marginals, equivalent to original kappa
(priors <-  apply(crossclass, 2, sum)/sum(crossclass))
#>         OP         SA         UA         UC         AV         AC 
#> 0.03225806 0.32616487 0.05376344 0.37634409 0.02867384 0.18279570 
tauW(crossclass, P=priors)$tau
#> [1] 0.6685984

# weighted accuracy; tau with equal priors
summaryTauW(weighted <- tauW(crossclass, W=weights))
#> [1] Cross-classification matrix:
#>      OP SA UA UC AV AC
#> [1,]  2  1  0  5  0  0
#> [2,]  1 74  2  1  3  6
#> [3,]  0  5  8  6  1  3
#> [4,]  6  1  3 91  0  0
#> [5,]  0  4  0  0  0  4
#> [6,]  0  6  2  2  4 38
#> [1] Number of observations: 279
#> [1] Weights:
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] 1.00 0.05 0.05 0.15 0.05 0.15
#> [2,] 0.05 1.00 0.05 0.05 0.05 0.35
#> [3,] 0.05 0.05 1.00 0.20 0.15 0.15
#> [4,] 0.15 0.05 0.25 1.00 0.10 0.25
#> [5,] 0.05 0.10 0.15 0.10 1.00 0.15
#> [6,] 0.20 0.30 0.10 0.25 0.20 1.00
#> [1] Overall accuracy (unweighted): 0.7634
#> [1] Overall accuracy (weighted): 0.8039
#> [1] User's accuracy (unweighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2500 0.8506 0.3478 0.9010 0.0000 0.7308 
#> [1] User's accuracy (weighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.3500 0.8787 0.4370 0.9178 0.1250 0.7942 
#> [1] Producer's reliability (unweighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2222 0.8132 0.5333 0.8667 0.0000 0.7451 
#> [1] Producer's reliability (weighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.3278 0.8412 0.6033 0.8905 0.1375 0.8069 
#> [1] Reference class proportions:
#>     OP     SA     UA     UC     AV     AC 
#> 0.0323 0.3262 0.0538 0.3763 0.0287 0.1828 
#> [1] Observed class proportions:
#>     OP     SA     UA     UC     AV     AC 
#> 0.0287 0.3118 0.0824 0.3620 0.0287 0.1864 
#> [1] Prior class probabilities:
#> [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
#> [1] Tau (unweighted): 0.7161
#> [1] Tau (weighted): 0.7253

# weighted accuracy; tau with user's = producer's marginals
summaryTauW(tauW(crossclass, W=weights, P=priors))
#> [1] Cross-classification matrix:
#>      OP SA UA UC AV AC
#> [1,]  2  1  0  5  0  0
#> [2,]  1 74  2  1  3  6
#> [3,]  0  5  8  6  1  3
#> [4,]  6  1  3 91  0  0
#> [5,]  0  4  0  0  0  4
#> [6,]  0  6  2  2  4 38
#> [1] Number of observations: 279
#> [1] Weights:
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] 1.00 0.05 0.05 0.15 0.05 0.15
#> [2,] 0.05 1.00 0.05 0.05 0.05 0.35
#> [3,] 0.05 0.05 1.00 0.20 0.15 0.15
#> [4,] 0.15 0.05 0.25 1.00 0.10 0.25
#> [5,] 0.05 0.10 0.15 0.10 1.00 0.15
#> [6,] 0.20 0.30 0.10 0.25 0.20 1.00
#> [1] Overall accuracy (unweighted): 0.7634
#> [1] Overall accuracy (weighted): 0.8039
#> [1] User's accuracy (unweighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2500 0.8506 0.3478 0.9010 0.0000 0.7308 
#> [1] User's accuracy (weighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.3500 0.8787 0.4370 0.9178 0.1250 0.7942 
#> [1] Producer's reliability (unweighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.2222 0.8132 0.5333 0.8667 0.0000 0.7451 
#> [1] Producer's reliability (weighted):
#>     OP     SA     UA     UC     AV     AC 
#> 0.3278 0.8412 0.6033 0.8905 0.1375 0.8069 
#> [1] Reference class proportions:
#>     OP     SA     UA     UC     AV     AC 
#> 0.0323 0.3262 0.0538 0.3763 0.0287 0.1828 
#> [1] Observed class proportions:
#>     OP     SA     UA     UC     AV     AC 
#> 0.0287 0.3118 0.0824 0.3620 0.0287 0.1864 
#> [1] Prior class probabilities:
#> [1] 0.03225806 0.32616487 0.05376344 0.37634409 0.02867384 0.18279570
#> [1] Tau (unweighted): 0.6686
#> [1] Tau (weighted): 0.6744

# change in accuracy statistics weighted vs. non-weighted
(weighted$overall.weighted - weighted$overall.naive)
#> [1] 0.04050179
(weighted$user.weighted - weighted$user.naive)
#>         OP         SA         UA         UC         AV         AC 
#> 0.10000000 0.02816092 0.08913043 0.01683168 0.12500000 0.06346154 
(weighted$prod.weighted - weighted$prod.naive)
#>         OP         SA         UA         UC         AV         AC 
#> 0.10555556 0.02802198 0.07000000 0.02380952 0.13750000 0.06176471 
```
