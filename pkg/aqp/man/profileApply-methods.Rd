\name{profileApply-methods}
\docType{methods}
\alias{profileApply}
\alias{profileApply,SoilProfileCollection-method}
\alias{splitProfiles}
\alias{splitProfiles,SoilProfileCollection-method}

\title{Apply a function to soil profiles within a SoilProfileCollection object.}
\description{Apply a function to soil profiles within a SoilProfileCollection object, each iteration has access to a SoilProfileCollection object.}
\section{Methods}{
\describe{

\item{\code{signature(object = "SoilProfileCollection")}}{}
}}

\value{A vector of length \code{nrow(object)} (horizon data) or of length \code{length(object)} (site data).}

\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom

# split a SPC into a list of single-profile SPC objects
# used internally by profileApply()
str(splitProfiles(sp1), 1)

# scale properties within each profile
# scaled = (x - mean(x)) / sd(x)
sp1$d <- profileApply(sp1, FUN=function(x) round(scale(x$prop), 2))
plot(sp1, name='d')


# compute depth-wise differencing by profile
# note that our function expects that the column 'prop' exists
f <- function(x) { c(x$prop[1], diff(x$prop)) }
sp1$d <- profileApply(sp1, FUN=f)
plot(sp1, name='d')

# compute depth-wise cumulative sum by profile
# note the use of an anonymous function
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
# note that we are only working with horizon data
# note that we are removing the 1st column, as it contains the profile ID
if(require(digest)) {
	s$md5 <- profileApply(s, function(x) digest(unlist(horizons(x)[, -1])))

	# get unique hashes
	u.md5 <- unique(s$md5)

	# list profile idx by hash:
	profiles.by.hash <- sapply(u.md5, function(i) which(s$md5 == i), simplify=FALSE)

	# get an index of the first copy of each profile
	u.profiles <- sapply(profiles.by.hash, function(x) x[1])

	# check: OK
	plot(s[u.profiles, ])
}


##
## helper functions: these must be modified to suit your own data
##

# compute the weighted-mean of some property within a given diagnostic horizon
# note that this requires conditional eval of data that may contain NA
# see ?slab and ?soil.slot for details on the syntax
# note that function expects certain columns within 'x'
f.diag.wt.prop <- function(x, d.hz, prop) {
	# extract diagnostic horizon data
	d <- diagnostic_hz(x)
	# subset to the requested diagnostic hz
	d <- d[d$diag_kind == d.hz, ]
	# if missing return NA
	if(nrow(d) == 0)
		return(NA)
	
	# extract depths and check for missing
	sv <- c(d$featdept, d$featdepb)
	if(any(is.na(sv)))
		return(NA)
	
	# create formula from named property
	fm <- as.formula(paste('~', prop))
	# return just the (weighted) mean, accessed from @horizons
	s <- slab(x, fm, seg_vect=sv)$p.mean
	return(s)
}


# conditional eval of thickness of some diagnostic feature or horizon
# will return a vector of length(x), you can save to @site
f.diag.thickness <- function(x, d.hz) {
	# extract diagnostic horizon data
	d <- diagnostic_hz(x)
	# subset to the requested diagnostic hz
	d <- d[d$diag_kind == d.hz, ]
	# if missing return NA
	if(nrow(d) == 0)
		return(NA)
	
	# compute thickness
	thick <- d$featdepb - d$featdept
	return(thick)
}


# conditional eval of property within particle size control section
# makes assumptions about the SPC that is passed-in
f.psc.prop <- function(x, prop) {
  # these are accessed from @site
  sv <- c(x$psctopdepth, x$pscbotdepth)
  # test for missing PCS data
  if(any(is.na(sv)))
    return(NA) 
	
	# this should never happen... unless someone made a mistake
	# check to make sure that the lower PSC boundary is shallower than the depth
	if(sv[2] > max(x))
		return(NA)
	
  # create formula from named property
  fm <- as.formula(paste('~', prop))
  # return just the (weighted) mean, accessed from @horizons
  s <- slab(x, fm, seg_vect=sv)$p.mean
  return(s)
}

}
\keyword{methods}
\keyword{manip}
