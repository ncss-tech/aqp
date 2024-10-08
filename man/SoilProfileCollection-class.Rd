% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-SoilProfileCollection.R
\docType{class}
\name{SoilProfileCollection}
\alias{SoilProfileCollection}
\alias{SoilProfileCollection-class}
\title{An S4 object representation of a group of soil profiles.}
\usage{
SoilProfileCollection(
  idcol = "id",
  hzidcol = "hzID",
  depthcols = c("top", "bottom"),
  metadata = list(aqp_df_class = "data.frame", aqp_group_by = "", aqp_hzdesgn = "",
    aqp_hztexcl = "", stringsAsFactors = FALSE),
  horizons = data.frame(id = character(0), hzID = character(0), top = numeric(0), bottom
    = numeric(0), stringsAsFactors = FALSE),
  site = data.frame(id = character(0), stringsAsFactors = FALSE),
  diagnostic = data.frame(stringsAsFactors = FALSE),
  restrictions = data.frame(stringsAsFactors = FALSE)
)
}
\arguments{
\item{idcol}{character. Profile ID Column Name}

\item{hzidcol}{character. Horizon ID Column Name}

\item{depthcols}{character. length 2 Top and Bottom Depth Column Names}

\item{metadata}{list. metadata including data.frame class in use and depth units}

\item{horizons}{data.frame. An object inheriting from data.frame containing Horizon data.}

\item{site}{data.frame. An object inheriting from data.frame containing Site data.}

\item{diagnostic}{data.frame. An object inheriting from data.frame containing diagnostic feature data. Must contain profile ID. See \code{diagnostic_hz()}}

\item{restrictions}{data.frame. An object inheriting from data.frame containing restrictive feature data. Must contain profile ID. See \code{restrictions()}}
}
\description{
In general, one should use \code{depths()} to initiate a SoilProfileCollection object from data. However, sometimes there are instances where either an empty, or very specific, object is needed. If that is the case, the general constructor \code{SoilProfileCollection} is available.
}
\details{
After aqp 2.0.2, the \verb{@sp} slot was removed from the SoilProfileCollection object. If you run into errors related to old object definitions, use \code{rebuildSPC()} on the problematic object.
}
\section{Slots}{

\describe{
\item{\code{idcol}}{character.}

\item{\code{hzidcol}}{character.}

\item{\code{depthcols}}{character.}

\item{\code{metadata}}{list.}

\item{\code{horizons}}{data.frame.}

\item{\code{site}}{data.frame.}

\item{\code{diagnostic}}{data.frame.}

\item{\code{restrictions}}{data.frame.}
}}

\examples{

## structure of default, empty SoilProfileCollection
str(SoilProfileCollection())


## use the depths() formula interface to specify
## profile ID, top and bottom depth and set up
## a SPC that is topologically correct and complete

d <- do.call('rbind', lapply(1:10, random_profile))

# promote to SoilProfileCollection and plot
depths(d) <- id ~ top + bottom
plot(d)

# split into new SoilProfileCollection objects by index
d.1 <- d[1, ]
d.2 <- d[2, ]
d.345 <- d[3:5, ]

# combine profile collections
# note that profiles are sorted according to ID
d.new <- c(d.345, d.1, d.2)
plot(d.new)

data(sp1)

## init SoilProfileCollection objects from data.frame
depths(sp1) <- id ~ top + bottom

## depth units
du <- depth_units(sp1)
depth_units(sp1) <- 'in'
depth_units(sp1) <- du

## horizon designation column
hzdesgnname(sp1) <- "name"
hzdesgnname(sp1)

## all designations in an SPC (useful for single profile SPC)
hzDesgn(sp1)

## horizon texture class column
hztexclname(sp1) <- "texture"
hztexclname(sp1)

## get/set metadata on SoilProfileCollection objects
# this is a 1-row data.frame
m <- metadata(sp1)
m$sampler <- 'Dylan'
metadata(sp1) <- m

## extract horizon data from SoilProfileCollection objects as data.frame
h <- horizons(sp1)

# also merge (left-join) of new columns and
# replacement of existing columns via horizons<-
horizons(sp1) <- h

# get number of horizons
nrow(sp1)


## getting site-level data
site(sp1)

## setting site-level data
# site-level data from horizon-level data (stored in @horizons)
site(sp1) <- ~ group


# make some fake site data, and append from data.frame
# a matching ID column must be present in both @site and new data
# note that IDs should all be character class
d <- data.frame(id=profile_id(sp1), p=runif(n=length(sp1)), stringsAsFactors=FALSE)
site(sp1) <- d

# edit horizon depths
horizonDepths(sp1) <- c('t', 'b')
horizonDepths(sp1)

# edit profile IDs
p <- sprintf("\%s-new", profile_id(sp1))
profile_id(sp1) <- p
profile_id(sp1)

}
\author{
Pierre Roudier, Dylan E. Beaudette, Andrew G. Brown
}
