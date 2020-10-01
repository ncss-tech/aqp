library(aqp)
library(crayon)

## great idea, inspired by Pierre Roudier

## TODO: use hex colors vs. munsell notation ?


fancySoilColorStyle <- function(cl, bar=FALSE, barWidth=10) {
  # convert to hex, NA-safe
  cols.full <- parseMunsell(cl)
  
  # pad NA
  NA.mask <- which(! is.na(cols.full))
  cols <- cols.full[NA.mask]
  
  # optionally encode color with a bar
  if(bar) {
    b <- paste(rep(' ', times=barWidth), collapse = '')
    cl <- rep(b, times=length(cl))
  }
  
  # make background styles
  # this relies on the limited number of available colors: typically 256
  # color-matching is done in sRGB space via Euclidian distance
  # low-chroma colors may appear "off"
  # see:
  #   crayon:::style_from_rgb
  #   crayon:::ansi256
  #
  res <- lapply(cols, make_style, bg=TRUE)
  
  # convert to HSV for text color selection
  V <- t(rgb2hsv(col2rgb(cols)))[, 3]
  
  # iterate over records and print styles
  for(i in seq_along(cols)) {
    # current text
    s.i <- style(cl[NA.mask][i], res[[i]])
    # make it bold
    s.i <- bold(s.i)
    # conditional text color based on HS{V}
    if(V[i] < 0.5) {
      s.i <- white(s.i)
    } else {
      s.i <- inverse(s.i)
    }
    
    # modify in place
    res[[i]] <- s.i
  }
  
  # init space accounting for NA
  res.full <- vector(mode = 'character', length = length(cols.full))
  res.full <- NA
  # convert to vector of styled text, interpolating NA
  res.full[NA.mask] <- unlist(res)
  
  return(res.full)
}


## example 1
# some colors in Munsell notation
cl <- c('10YR 3/3', NA, '2.5YR 5/8', '5BG 2/2', '2.5Y 2/7', '7.5YR 2/1', NA, '5YR 2/5')

# make styles
st <- fancySoilColorStyle(cl)

# print to terminal
cat(st, sep = '\n')


## example 2
# via attributes
d <- data.frame(id=1:length(cl), m=cl, stringsAsFactors = FALSE)

attr(d$m, 'fancyStyle') <- fancySoilColorStyle(d$m)

cat(attr(d$m, 'fancyStyle'), sep='\n')

## example 3
# another example: colors are somewhat washed out
data(sp6)
st <- fancySoilColorStyle(getClosestMunsellChip(sp6$color, convertColors = FALSE))
cat(st, sep = '\n')


## example 4: color bar
# some colors in Munsell notation
cl <- c('10YR 3/3', NA, '2.5YR 5/8', '5BG 2/2', '2.5Y 2/7', '7.5YR 2/1', NA, '5YR 2/5')
d <- data.frame(id=1:length(cl), m=cl, stringsAsFactors = FALSE)

# make styles as color bars
d$.colorBar <- fancySoilColorStyle(d$m, bar = TRUE)

# print to terminal
cat(d$.colorBar, sep = '\n')

## ... how do you print all of the columns nicely?
# knitr?
knitr::kable(d, row.names = FALSE, format = 'pandoc')

## ?
data(sp6)
sp6$.colorBar <- fancySoilColorStyle(getClosestMunsellChip(sp6$color, convertColors = FALSE), bar = TRUE)
knitr::kable(sp6[, c('id', 'top', 'bottom', 'name', 'color', '.colorBar')], row.names = FALSE, format = 'pandoc')
