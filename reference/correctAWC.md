# Apply rock fragment or salt correction to available water content

Apply rock fragment or salt correction to available water content

## Usage

``` r
correctAWC(
  awc,
  total_rf = numeric(length(awc)),
  gravel = NULL,
  ec = NULL,
  nullFragsAreZero = TRUE
)
```

## Arguments

- awc:

  Numeric vector of available water capacities (e.g. from `estimateAWC`)

- total_rf:

  Numeric vector of rock fragment volume percentage, 0 - 100

- gravel:

  Numeric vector of gravel volume percentage, 0 - 100

- ec:

  Numeric vector of electrical conductivity, mmhos/cm

- nullFragsAreZero:

  Interpret `NA` in `total_rf`, `gravel` or `ec` as `0`? Default: `TRUE`

## Value

A numeric vector (double) containing estimated available water
capacities corrected for rock fragments and salts

## Examples

``` r
# medium organic matter, loam texture 
base.awc <- 0.18 # estimateAWC(texcl = "l", omcl = 2, na.rm = TRUE)

# medium organic matter, loam texture w/ 23% rock fragments by volume 
corrected.awc <- correctAWC(base.awc, total_rf = 23)
corrected.awc
#> [1] 0.15

# medium organic matter, loam texture w/ 0% frags by volume and 8 mmhos/cm salts
salty.awc <- correctAWC(base.awc, total_rf = 0, ec = 8)
salty.awc
#> [1] 0.14
```
