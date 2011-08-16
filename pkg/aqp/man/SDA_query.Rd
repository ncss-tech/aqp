\name{SDA_query}
\alias{SDA_query}
\alias{cleanSDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Soil Data Access Query}
\description{Submit a query to the Soil Data Acccess (SDA) website in SQL, get the results as a dataframe.}
\usage{SDA_query(q)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{a text string containing valid SQL}
}
\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov}.}
\value{A dataframe containing the results.}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}
\note{This function requires the `SSOAP` package.}

\seealso{\code{\link{MUKEYS_by_ll_bbox}}, \code{\link{mapunit_geom_by_ll_bbox}}}
\examples{
# NASIS export metadata:
\dontrun{res <- SDA_query('select areasymbol, exportcertdate from distlegendmd order by areasymbol')}

# basic query:
\dontrun{res <- SDA_query("select cokey, compname, comppct_r from component where compname = 'yolo' and majcompflag = 'Yes' ")}

# get tabular data based on result from spatial query:
\dontrun{
# bbox
b <- c(-120.9,37.7,-120.8,37.8)
# get map unit keys for this bbox
m <- MUKEYS_by_ll_bbox(b)
# make an SQL-compliant "in" statment
in.statment <- paste(m, collapse="','")
in.statment <- paste("('", in.statment, "')", sep='')
# format query:
q <- paste("SELECT cokey, compname, comppct_r FROM component WHERE majcompflag = 'Yes' AND mukey IN ", in.statment, "ORDER BY mukey, comppct_r DESC", sep="")
# now get component data for these map unit keys
res <- SDA_query(q)
}

}

\keyword{manip}

