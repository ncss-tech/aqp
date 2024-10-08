% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SoilProfileCollection-operators.R
\docType{methods}
\name{[[}
\alias{[[}
\alias{[[,SoilProfileCollection-method}
\alias{[[,SoilProfileCollection,ANY-method}
\alias{[[,SoilProfileCollection,ANY,ANY-method}
\title{Get column of horizon or site data in a SoilProfileCollection}
\usage{
\S4method{[[}{SoilProfileCollection}(x, i, j)
}
\arguments{
\item{x}{a SoilProfileCollection}

\item{i}{an expression resolving to a single column name in site or horizon table}

\item{j}{(not used)}
}
\description{
Get the data from a column accessed by name. Column names other than profile ID are not shared between site and horizons.
Bonus: \code{[[} gives access to all site and horizon level variables in tab complete for RStudio using the magrittr pipe operator!
}
\examples{

data(sp2)
depths(sp2) <- id ~ top + bottom
site(sp2) <- ~ surface

# get with [[
sp2[['surface']]

# get using "unknown" expression:
#  "2nd + 3rd horizon column names"
for(i in horizonNames(sp2)[2:3])
 print(sp2[[i]])

data(sp5)

# some column names to work with
rgb.columns <- c("R25","G25","B25")

res <- lapply(rgb.columns, function(x) {

  # [[ allows you to access column names in a loop
  round(sp5[[x]] * 255)

})

# rename scaled results
names(res) <- paste0(rgb.columns,"_scl")

# add horizon ID to results
result <- data.frame(hzID = hzID(sp5), do.call('cbind', res))
head(result)

# join result back into horizons
horizons(sp5) <- result
}
