# Horizon Transition Probabilities

Functions for creating and working with horizon (sequence) transition
probability matrices.

See the following tutorials for some ideas:

- [horizon designation
  TP](http://ncss-tech.github.io/AQP/aqp/hz-transition-probabilities.md)

- [soil color
  TP](http://ncss-tech.github.io/AQP/aqp/series-color-TP-graph.md)

## Usage

``` r
hzTransitionProbabilities(
  x,
  name = GHL(x, required = TRUE),
  loopTerminalStates = FALSE
)

mostLikelyHzSequence(mc, t0, maxIterations = 10)
```

## Arguments

- x:

  a `SoilProfileCollection` object.

- name:

  A horizon level attribute in `x` that names horizons.

- loopTerminalStates:

  should terminal states loop back to themselves?

  This is useful when the transition probability matrix will be used to
  initialize a `markovchain` object. See examples below.

- mc:

  Passed to `markovchain` `conditionalDistribution()`

- t0:

  Passed to `markovchain` `conditionalDistribution()`

- maxIterations:

  Maximum number of iterations. Default: `10`

## Value

A square matrix of transition probabilities. See examples.

The function
[`genhzTableToAdjMat()`](https://ncss-tech.github.io/aqp/reference/genhzTableToAdjMat.md)
returns a square adjacency matrix. See examples.

The function `mostLikelyHzSequence()` returns the most likely sequence
of horizons, given a `markovchain` object initialized from horizon
transition probabilities and an initial state, `t0`. See examples.

## Note

These functions are still experimental and subject to change.

## See also

[`generalize.hz()`](https://ncss-tech.github.io/aqp/reference/generalize.hz.md)

## Author

D.E. Beaudette

## Examples

``` r
data(sp4)
depths(sp4) <- id ~ top + bottom

# horizon transition probabilities: row -> col transitions
(tp <- hzTransitionProbabilities(sp4, 'name'))
#> Warning: ties in transition probability matrix
#>     A A1 A2 AB       ABt        Bt       Bt1 Bt2 Bt3
#> A   0  0  0  0 0.1111111 0.4444444 0.4444444   0   0
#> A1  0  0  1  0 0.0000000 0.0000000 0.0000000   0   0
#> A2  0  0  0  1 0.0000000 0.0000000 0.0000000   0   0
#> AB  0  0  0  0 0.0000000 0.0000000 1.0000000   0   0
#> ABt 0  0  0  0 0.0000000 0.0000000 1.0000000   0   0
#> Bt  0  0  0  0 0.0000000 0.0000000 0.0000000   0   0
#> Bt1 0  0  0  0 0.0000000 0.0000000 0.0000000   1   0
#> Bt2 0  0  0  0 0.0000000 0.0000000 0.0000000   0   1
#> Bt3 0  0  0  0 0.0000000 0.0000000 0.0000000   0   0
#> attr(,"ties")
#> [1] TRUE


```
