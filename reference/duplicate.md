# Duplicate Profiles of a SoilProfileCollection

A simple function to duplicate the contents of a `SoilProfileCollection`
object. Old profile IDs are saved as a site-level attribute (`oldID`)
and new IDs are generated using a numeric serial number.

## Usage

``` r
duplicate(x, times = 3, oldID = ".oldID")
```

## Arguments

- x:

  a `SoilProfileCollection` object with 1 or more profiles

- times:

  requested number of copies

- oldID:

  site-level attribute used to store the original profile IDs

## Value

a `SoilProfileCollection` object

## Author

D.E. Beaudette

## Examples

``` r
# sample data
data('sp4')

# promote to SPC
depths(sp4) <- id ~ top + bottom

# duplicate each profile 2 times
d <- duplicate(sp4, times = 2)

# graphical check
par(mar = c(0, 0, 3, 1))
plotSPC(d, color = 'Ca', width = 0.25)

```
