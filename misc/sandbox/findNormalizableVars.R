library(aqp, warn = FALSE)
library(data.table)

# number of random sites
n <- 10

# make some horizon data with denormalized spatial info
randomsite <- data.table(
  id = 1:n,
  x = runif(n),
  y = runif(n),
  aletter = LETTERS[floor(runif(n, 1, 26) + 1)]
)
spc <- data.frame(randomsite[,"id"][, 
                             random_profile(.I), by = id][
                               randomsite, on = "id"])

depths(spc) <- id ~ top + bottom

# coordinates<- does basic normalization from the model.frame output
coordinates(spc) <- ~ x + y

# still need to use site<-~ for other variables
# site(spc) <- ~ aletter

plot(slot(spc, 'sp'))

.findNormalizableVars <- function(object) {
  h <- horizons(object)
  horizonNames(object)[sapply(horizonNames(object), function(x)
    all(aggregate(h[[x]], by = list(h[[idname(object)]]),
                  function(y) length(unique(y)) == 1)$x))]
}

.findNormalizableVars_2 <- function(object) {
  h <- data.table(horizons(object))
  
  # copy internal id
  h$.internalID <- h[[idname(object)]]
  
  idx <- apply(h[, lapply(.SD, function(x) length(unique(x))), 
                 by = .internalID], 
               MARGIN = 2, 
               function(y) all(y == 1))
  
  # remove internal ID
  idx <- idx[-1]
  
  # lookup in horizon names
  horizonNames(object)[idx]
}

bench::mark(.findNormalizableVars(spc),
            .findNormalizableVars_2(spc))
