# Visual Explanation for `plotSPC`

Create a visual explanation for the many arguments to `plotSPC`. Call
this function instead of `plotSPC`, all objects after `x` are passed on
to `plotSPC`. Nearly all of the figures in the [Introduction to
SoilProfileCollection Objects
tutorial](https://ncss-tech.github.io/AQP/aqp/aqp-intro.html) are
created with this function.

## Usage

``` r
explainPlotSPC(x, ...)
```

## Arguments

- x:

  a `SoilProfileCollection` object

- ...:

  arguments passed to
  [`plotSPC`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)

## Value

a list of internally-used ordering vectors and graphical offsets /
scaling factors

## See also

[`plotSPC`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)

## Author

D.E. Beaudette

## Examples

``` r
# sample data
data(sp4)
depths(sp4) <- id ~ top + bottom

# proposed vector of relative positions, overlap likely
pos <- c(1, 1.1, 3, 4, 5, 5.2, 7, 8, 9, 10)

# try it
explainPlotSPC(sp4, name = 'name', relative.pos=pos)


# attempt to fix using an integer sequence, short-circut will prevent adjustments
explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(1:10))


# attempt to adjust using defaults
explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos))
#> 28 iterations


# attempt to adjust and tinker with defaults
explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, adj = 0.2))
#> 51 iterations


# enforce larger space between
explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, thresh = 0.7))
#> 28 iterations


# more complex adjustments required
pos <- c(1, 2, 3, 3.3, 5, 5.1, 5.5, 8, 9, 10)

# tinker
explainPlotSPC(sp4, name = 'name', relative.pos = pos)

explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos))
#> 17 iterations


explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, 
thresh = 0.7))
#> 103 iterations


explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, 
thresh = 0.7, adj = 0.2))
#> 333 iterations


# SANN: solution requires many iterations, and will not always converge
explainPlotSPC(sp4, name = 'name', 
relative.pos = fixOverlap(pos, thresh = 0.85, adj = 0.2)
)
#> 65 iterations


# electrostatics: solution requires larger charge (q)
explainPlotSPC(sp4, name = 'name', 
relative.pos = fixOverlap(pos, thresh = 0.85, method = 'E', q = 2)
)
#> 5 iterations

```
