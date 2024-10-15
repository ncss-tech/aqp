library(aqp)

# example data
data("jacobs2000")

# local copy
g <- jacobs2000

# spike some horizon colors with green / blue hues
g$matrix_color_munsell[4] <- '5G 4/6'
g$matrix_color_munsell[29] <- '5B 4/6'
g$matrix_color_munsell[36] <- '5R 4/6'

# horizon correlation patterns
# applied to horizon desingation
a_pattern <- c(`A` = "^A",
               `E` = "E", 
               `Bt` = "[B]+t", 
               `Bh` = "[B]+h", 
               `C` = "^C", 
               `foo` = "bar")


# safe wrapper around mixMunsell()
mixFun <- function(x, top, bottom) {
  # weights
  w <- bottom - top
  
  # index to non-NA values
  .idx <- which(! is.na(x))
  .n <- length(x[.idx])
  
  # if all NA, return NA
  if(.n < 1) {
    return(NA)
    
    # if only a single color, return that  
  } else if (.n == 1){
    print('just 1!')
    return(x[.idx])
    
  } else {
    # mix colors, retain only munsell notation
    .res <- mixMunsell(x[.idx], w[.idx], mixingMethod = 'exact')$munsell 
    return(.res)
  }
}

# collapse according to patterns
m <- collapseHz(g,
                pattern = a_pattern,
                AGGFUN = list(
                  matrix_color_munsell = mixFun
                )
)

# new profile IDs so we can safely combine with source data
profile_id(m) <- sprintf("%s-c", profile_id(m))

# combine
z <- c(g, m)

# convert Munsell colors -> sRGB in hex notation
z$soilcolor <- parseMunsell(z$matrix_color_munsell)

# plot combined collection
par(mar = c(0, 0, 0, 3))
plotSPC(z, color = 'soilcolor', name = 'name', name.style = 'center-center', width = 0.35, cex.names = 0.75)

## start fresh

# combine all horizons by profile

g <- jacobs2000
horizons(g)$.all <- 'soil'
collapseHz(g, by = '.all')


m <- collapseHz(g,
                by = '.all',
                AGGFUN = list(
                  matrix_color_munsell = mixFun
                )
)

profile_id(m) <- sprintf("%s-c", profile_id(m))
z <- c(g, m)
z$soilcolor <- parseMunsell(z$matrix_color_munsell)

# neat
par(mar = c(0, 0, 0, 3))
plotSPC(z, color = 'soilcolor', name = 'name', name.style = 'center-center', width = 0.35, cex.names = 0.75)


