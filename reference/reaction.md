# Convert pH to/from Reaction Classes

Convert pH to/from Reaction Classes

## Usage

``` r
ph_to_rxnclass(x, halfclass = FALSE, as.is = FALSE, droplevels = TRUE)

rxnclass_to_ph(x, halfclass = FALSE, digits = 2, simplify = TRUE)

ReactionClassLevels(halfclass = FALSE, as.is = FALSE)
```

## Arguments

- x:

  input pH values (numeric; `ph_to_rxnclass()`) or reaction classes
  (character; `rxnclass_to_ph()`)

- halfclass:

  Split the standard classes in half for higher resolution? Default:
  `FALSE`

- as.is:

  logical. Should character vectors be converted to factors? Default:
  `FALSE`

- droplevels:

  logical. Drop unused levels in factors? Default: `FALSE`

- digits:

  Number of digits after decimal place; Default: `2`. Used only for
  `rxnclass_to_ph()`

- simplify:

  Simplify list result to numeric vector when length of result is 1?
  Default: `TRUE`

## Value

`ph_to_rxnclass()`: a vector of reaction classes corresponding to
numeric input in `x`; if `as.is=FALSE` an ordered factor using
`ReactionClassLevels()`

`rxnclass_to_ph()`: a list of data.frame objects containing high/low
values of reaction class 1:1 with input; if simplify=TRUE and input is a
data.frame.

`ReactionClassLevels()`: ordered factor containing descriptive terms for
reaction classes

## Examples

``` r
ph_to_rxnclass(6.2)
#> [1] slightly acid
#> Levels: slightly acid
rxnclass_to_ph("slightly acid")
#>   pH_low pH_high
#> 1   6.05    6.55

rxnclass_to_ph(list(c("Slightly Acid", NA, "Moderately Acid"),
                    c("Slightly Acid", NA, "Strongly Acid")), simplify = FALSE)
#> [[1]]
#>   pH_low pH_high
#> 1   5.55    6.55
#> 
#> [[2]]
#>   pH_low pH_high
#> 1   5.05    6.55
#> 
ReactionClassLevels()
#>  [1] ultra acid             extremely acid         very strongly acid    
#>  [4] strongly acid          moderately acid        slightly acid         
#>  [7] neutral                slightly alkaline      moderately alkaline   
#> [10] strongly alkaline      very strongly alkaline
#> 11 Levels: ultra acid < extremely acid < ... < very strongly alkaline
```
