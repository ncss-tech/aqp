\name{profileApply-methods}
\docType{methods}
\alias{profileApply}
\alias{profileApply,SoilProfileCollection-method}
\alias{split,SoilProfileCollection-method}

\title{Apply a function to soil profiles within a SoilProfileCollection object.}
\description{Apply a function to soil profiles within a SoilProfileCollection object. Note that each iteration has access to a SoilProfileCollection object.}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{}
}}

\value{A vector of length \code{nrow(object)} (horizon data) or of length \code{length(object)} (site data).}

\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom

# scale properties within each profile
# scaled = (x - mean(x)) / sd(x)
sp1$d <- profileApply(sp1, FUN=function(x) round(scale(x$prop), 2))
plot(sp1, name='d')

# split a SPC into a list of single-profile SPC objects
str(split(sp1), 1)


# compute depth-wise differencing by profile
# note that our function expects that the column 'prop' exists
f <- function(x) { c(x$prop[1], diff(x$prop)) }
sp1$d <- profileApply(sp1, FUN=f)
plot(sp1, name='d')

# compute depth-wise cumulative sum by profile
# note the use of an anonymous function function
sp1$d <- profileApply(sp1, FUN=function(x) cumsum(x$prop))
plot(sp1, name='d')


# compute profile-means, and save to @site
# there must be some data in @site for this to work
site(sp1) <- ~ group
sp1$mean_prop <- profileApply(sp1, FUN=function(x) mean(x$prop, na.rm=TRUE))

# re-plot using ranks defined by computed summaries (in @site)
plot(sp1, plot.order=rank(sp1$mean_prop))

## use the digest library to detect duplicate data
data(sp1)

# make a copy, stack, and give new IDs
s.1 <- sp1 
s.2 <- sp1
s.2$id <- paste(s.2$id, '-copy', sep='')
s <- rbind(s.1, s.2)
depths(s) <- id ~ top + bottom
plot(s)

# setup site data, so that we can save md5 hash to @site later
site(s) <- ~ group

# eval dupes with digest, save md5 hash into @site
if(require(digest)) {
	s$md5 <- profileApply(s, function(x) digest(unlist(x[, -1])))

	# get unique hashes
	u.md5 <- unique(s$md5)

	# list profile idx by hash:
	profiles.by.hash <- sapply(u.md5, function(i) which(s$md5 == i), simplify=FALSE)

	# get an index of the first copy of each profile
	u.profiles <- sapply(profiles.by.hash, function(x) x[1])

	# check: OK
	plot(s[u.profiles, ])
}


}
\keyword{methods}
\keyword{manip}
