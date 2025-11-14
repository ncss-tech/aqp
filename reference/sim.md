# DEPRECATED Simulate Soil Profiles

Simulate a collection of soil profiles based on the horizonation of a
single soil profile. Now deprecated: use
[`perturb()`](https://ncss-tech.github.io/aqp/reference/perturb.md) for
perturbations of horizon thicknesses or boundaries.

## Usage

``` r
sim(x, n = 1, iterations = 25, hz.sd = 2, min.thick = 2)
```

## Arguments

- x:

  a SoilProfileCollection object containing a single profile from which
  to draw simulated data

- n:

  the number of requested simulations

- iterations:

  sampling iterations used to determine each horizon thickness

- hz.sd:

  standard deviation used to simulate horizon thickness, can be a vector
  but must divide evenly into the number of horizons found in `x`

- min.thick:

  minimum horizon thickness allowed in simulation results

## Value

A SoilProfileCollection object with `n` simulated profiles, each
containing the same number of horizons and same data as `x`

## Details

This function generates a collection of simulated soil profiles based on
the horizon thickness data associated with a single "template" profile.
Simulation is based on sampling from a family of Gaussian distribution
with means defined by the "template" profile and standard deviation
defined by the user.

## See also

[`random_profile`](https://ncss-tech.github.io/aqp/reference/random_profile.md)
[`perturb`](https://ncss-tech.github.io/aqp/reference/perturb.md)

## Author

D. E. Beaudette

## Examples

``` r
# please see documentation for perturb() for examples
#  the sim() function calls perturb() internally
```
