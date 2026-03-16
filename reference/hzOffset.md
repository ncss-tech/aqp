# Select Horizons Above or Below a Reference Horizon

These functions return "selected" horizons offset from "reference"
horizon defined by logical expressions which include horizon data
elements. Typically, `hzAbove()` and `hzBelow()` are used as a simpler
interface to `hzOffset()`. Selected horizons can be returned as a
complete `SoilProfileCollection` (`SPC = TRUE`), a list of horizon data
row indices (`SPC = FALSE, simplify = FALSE`), or as a numeric vector of
horizon data row indices (`SPC = FALSE, simplify = TRUE`).

## Usage

``` r
hzAbove(x, ..., offset = NULL, SPC = TRUE, simplify = SPC, single = FALSE)

hzBelow(x, ..., offset = NULL, SPC = TRUE, simplify = SPC, single = FALSE)

hzOffset(x, hzidx, offset, SPC = FALSE, simplify = TRUE)
```

## Arguments

- x:

  `SoilProfileCollection`

- ...:

  comma-separated set of logical expressions, in context of horizon
  data, length for each expression must match number of horizons in `x`

- offset:

  integer vector or NULL, when specified this is the `[, j]`
  (horizon/slice) index definining the horizons above or below a
  reference horizon that should be returned. see details

- SPC:

  logical, return a `SoilProfileCollection` subset to selected horizons

- simplify:

  logical, flatten list of horizon row indices into a numeric vector

- single:

  logical, interpret multiple matching reference horizons as a single
  horizon, defined by max(top) - min(bottom)

- hzidx:

  integer vector of target horizon row indices, typically calculated by
  `hzAbove()` and `hzBelow()`

## Value

One of:

- `SoilProfileCollection`: when `SPC = TRUE`

- list of horizon row indices when `SPC = FALSE` and `simplify = FALSE`

- vector of horizon row indices: when `SPC = FALSE` and
  `simplify = TRUE`

The return value will not include data from profiles which have no
matching reference horizons.

## Details

Expressions that match multiple reference horizons will result in the
inclusion of these reference horizons in the selected horizons.
Interpreting multiple reference horizons as a single horizon
(`single = TRUE`), defined by min(top) - max(bottom), will ensure that
only selected horizons are returned.

The `offset` argument controls which "horizons above" or "horizons
below" reference horizon(s) are selected, using the
`SoilProfileCollection` `[, j]` (horizon/slice) notation. Possible
values and their interpretation are as follows:

- `offset = NULL`: select all horizons above or below reference
  horizon(s)

- `offset = 1`: select the first horizon above or below reference
  horizon(s)

- `offset = 1:2`: select the first and second horizons above or below
  reference horizon(s)

- `offset = 1:10`: safely select the 1st -\> 10th horizons, even if
  there are fewer, above or below reference horizon(s)

## Examples

``` r
# example data
x <- c(
  'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
  'P2:Ap|AA|E|BhsBhs|Bw1Bw1|CCCCC',
  'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
  'P4:AA|EEE|BhsBhsBhsBhs|BwBw|CCCCC',
  'P5:AAAA|ACACACACAC|CCCCCCCCCCC|CdCdCd'
)

s <- quickSPC(x)

# single match for most profiles
.ex <- grepl('Bt3|Bw', s$name)
s$e <- .ex

# default: select all horizons above or below
a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found

op <- par(no.readonly = TRUE)
par(mar = c(0, 0, 3, 0))
plotSPC(
  s, color = 'e', col.label = 'reference', col.palette = c('grey', 'royalblue'), 
  name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)


# highlight selected horizons above and below with brackets
addBracket(
  depths(s, hzID = FALSE)[a, ], agg = TRUE,
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)
#> Error: object 'a' not found

addBracket(
  depths(s, hzID = FALSE)[b, ], agg = TRUE,
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)
#> Error: object 'b' not found


# select only 1st horizon above / below
a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE, offset = 1)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE, offset = 1)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found

plotSPC(
  s, color = 'e', col.label = 'reference', col.palette = c('grey', 'royalblue'), 
  name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)

# highlight selected horizons above and below with brackets
addBracket(
  depths(s, hzID = FALSE)[a, ], agg = TRUE,
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)
#> Error: object 'a' not found

addBracket(
  depths(s, hzID = FALSE)[b, ], agg = TRUE,
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)
#> Error: object 'b' not found


# multiple matches
.ex <- grepl('B', s$name)
s$e <- .ex

# default
a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found

par(mfcol = c(1, 2))
plotSPC(
  s,  col.label = 'reference', color = 'e', 
  col.palette = c('grey', 'royalblue'), 
  name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)

addBracket(
  depths(s, hzID = FALSE)[a, ], agg = TRUE,
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)
#> Error: object 'a' not found

addBracket(
  depths(s, hzID = FALSE)[b, ], agg = TRUE,
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)
#> Error: object 'b' not found

mtext('single = FALSE',  side = 1, line = -1.5, at = 0, adj = -0.5)


# interpret multiple reference hz as a single reference hz
a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found


plotSPC(
  s, col.label = 'reference', color = 'e', 
  col.palette = c('grey', 'royalblue'), name = 'name', 
  hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)

addBracket(
  depths(s, hzID = FALSE)[a, ], agg = TRUE,
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)
#> Error: object 'a' not found

addBracket(
  depths(s, hzID = FALSE)[b, ], agg = TRUE,
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)
#> Error: object 'b' not found

mtext('single = TRUE',  side = 1, line = -1.5, at = 0, adj = -0.5)



# demonstrate SPC = TRUE, single = TRUE
plotSPC(
  s, col.label = 'reference', color = 'e', 
  col.palette = c('grey', 'royalblue'), name = 'name', 
  hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75, max.depth = 250
)

a <- hzAbove(s, .ex, SPC = TRUE, single = TRUE)
#> Error in eval(.dots[[i]], .data, parent.frame(n = 2)): object '.ex' not found

plotSPC(
  a, name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75, max.depth = 250
)
#> Error: object 'a' not found
title('selected profiles/horizons')

par(op)

```
