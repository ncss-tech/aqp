# Guess Horizon Slot Column Names

`guessHzAttrName()`: Guess the horizon column name where
possible/preferred formative elements are known. There is a preference
for records where more optional requirements are met to handle cases
where there will be many matches. For example, working with soil data
one might have "low, RV and high" total clay, as well as clay fractions.
One could distinguish between these different measurements using
standard formative elements for column names from the database of
interest. Result is the first match in `horizonNames(x)` with the most
required plus optional patterns matched.

e.g. `guessHzAttrName(x, attr="clay", optional=c("total", "_r"))`
matches (`claytotal_r == totalclay_r`) over
(`clay_r == claytotal == totalclay`) over `clay`.

`guessHzDesgnName()`: **DEPRECATED** This follows the historic
convention used by
[`aqp::plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)
looking for "hzname" or other column names containing the regular
expression "name". If the pattern "name" is not found, the pattern
"desgn" is searched as a fallback, as "hzdesgn" or "hz_desgn" are other
common column naming schemes for horizon designation name.

`guessHzTexClName()`: **DEPRECATED** This function is used to provide a
texture class attribute column name to functions. It will use regular
expressions to match "texcl" which is typically the texture of the fine
earth fraction, without modifiers or in-lieu textures. Alternately, it
will match "texture" for cases where "texcl" is absent (e.g. in NASIS
Component Horizon).

## Usage

``` r
guessHzAttrName(x, attr, optional = NULL, verbose = TRUE, required = FALSE)

guessHzDesgnName(x, required = FALSE)

guessHzTexClName(x, required = FALSE)
```

## Arguments

- x:

  A SoilProfileCollection

- attr:

  *character*. A regular expression containing required formative
  element of attribute name.

- optional:

  *character*. Vector of regular expression(s) containing optional
  formative elements of attribute name.

- verbose:

  *logical*. Produce message output about guesses? Default: `TRUE`

- required:

  logical Default: `FALSE`. Is this attribute required? If it is, set to
  `TRUE` to trigger error on invalid value.

## Value

Character containing horizon attribute column name. Result is the first
match in `horizonNames(x)` with the most required plus optional patterns
matched.

## Author

Andrew G. Brown

## Examples

``` r
# a has the required attr pattern, but none of the optional
a <- data.frame(id = 1, top = c(0,10), bottom=c(10,40),
                clay=c(18,19))
depths(a) <- id ~ top + bottom

guessHzAttrName(a, attr="clay", optional=c("total", "_r"))
#> guessing horizon attribute 'clay' is stored in `clay`
#> [1] "clay"

# b has requried attr pattern, and one of the opional patterns
#   notice that it also contains "clay" but preferentially matches more optional patterns
b <- data.frame(id = 1, top = c(0,10), bottom=c(10,40),
                clay=c(0.18,0.19), clay_r=c(18,19))
depths(b) <- id ~ top + bottom

guessHzAttrName(b, attr="clay", optional=c("total", "_r"))
#> guessing horizon attribute 'clay' is stored in `clay_r`
#> [1] "clay_r"

# c has total and _r (both optional) on either side of clay
# having all of the optional patterns plus required is best evidence, and first
# column containing that combination will be returned
c <- data.frame(id = 1, top = c(0,10), bottom=c(10,40),
                totalclay_r=c(18,19), claytotal_r=c(0.18,0.19))
depths(c) <- id ~ top + bottom

guessHzAttrName(c, attr="clay", optional=c("total", "_r"))
#> guessing horizon attribute 'clay' is stored in `totalclay_r`
#> [1] "totalclay_r"
```
