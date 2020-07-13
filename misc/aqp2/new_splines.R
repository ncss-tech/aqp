# making use of @obrl-soil's new mpspline2 package [soon to be on CRAN!]
#
# author: andrew g brown; 2020/07/12
#
# this is an attempt at creating a generic method for mass-preserving / equal-area splines
# on aqp SoilProfileCollection (SPC) data that may contain a large amount of missing values.
#
# it also provides an avenue to get 1cm continuous spline output back in a handy form (1cm sliced SPC).
#
# In the presence of missing values, getting the result back into the SPC is not always easy.
#
# The example code that follows should illustrate the major indexing pitfalls you need to
# be aware of.
#
# I have tried to cover corner cases so please let me know if you find a way to break it!
#
# get development versions of soil packages + dependencies
# remotes::install_github(c("ncss-tech/aqp",
#                "ncss-tech/soilDB",
#                "obrl-soil/mpspline2"))

# load packages
library(aqp) # for SoilProfileCollection object + wrangling code
library(soilDB) # sample data
library(mpspline2) # generic mass-preserving/equal area spline implementation

#' Missing-data-safe, SPC-wide wrapper around mpspline2::mpspline "continuous" 1cm output
#'
#' @name do_aqp_mpspline
#'
#' @description Facilitate safe use of just about any numeric SPC horizon attribute, from any SPC, with \code{mpspline2::mpspline}.
#'
#' This function will automatically filter profiles with \code{NA} in attribute of interest.
#'
#' This may be more conservative filtering than you expect, with intention of splines over a constant interpolation interval [with essentially no gaps].
#'
#' This is all with an eye toward aggregating many profile-level splines together where missing data is hard to reason over.
#'
#' Data completeness is assessed and the input SPC is filtered and truncated to create a container for the 1cm results from \code{mpspline2::mpspline}.
#'
#' @param object A SoilProfileCollection
#' @param var_name Column name in \code{@horizons} slot of \code{object} containing numeric values to spline
#' @param pattern Regex pattern to match for bottom of profile (passed to estimateSoilDepth) default: "R|Cr|Cd|qm"
#' @param hzdesgn Column name in \code{@horizons} slot of \code{object} containing horizon designations default: \code{aqp::guessHzDesgnName(object)}
#' @param ... Additional arguments to \code{mpspline2::mpspline}
#'
#' @author Andrew G. Brown
#'
#' @return A SoilProfileCollection with 1cm slices. Spline variables are in columns prefixed with "spline_" and RMSE/RMSE_IQR are in colums prefixed with "rmse_". If any profiles were removed from the collection, their profile IDs are stored in attr(result, 'removed').
#'
#' @export do_aqp_mpspline
#'
#' @examples
#'
#' library(aqp)
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' res <- do_aqp_mpspline(sp1, "prop")
#' plotSPC(res[1:5,], color = "spline_prop", divide.hz = FALSE)
#'
do_aqp_mpspline <- function(object, var_name = NULL,
                            pattern = "R|Cr|Cd|qm", hzdesgn = guessHzDesgnName(object),
                            ...) {

  if(is.null(var_name) | !(var_name %in% horizonNames(object)))
    stop("argument `var_name` must specify a single horizon-level variable", call.=FALSE)

  hztop <- horizonDepths(object)[1]
  hzbot <- horizonDepths(object)[2]

  # glom to "available interval" in each profile
  # NOTE: we will handle warnings (profiles with no data at all) at end
  spc.sub <- suppressWarnings(glomApply(object, function(p) {
    i <- which(diff(c(0, cumsum(!is.na(p[[var_name]])))) == 1)
    h <- horizons(p)
    # basically this excludes NA values at top and bottom of profile
    # (O horizons, bedrock) but wont check missing values inbetween
    # need at least two horizons to make a spline
    if(length(i) < 2)
      return(c(0,0))
    top_depth <- h[[hztop]][i[1]]
    bot_depth <- h[[hzbot]][i[length(i)]]
    return(c(top_depth, bot_depth))
  }))

  # debug : inspect  horizon values for var_name
  #plot(spc.sub[1:10,], color=var_name)

  # only take profiles that have 100% data coverage in above interval
  #  i.e. if a single horizon is missing data, remove whole profile
  spc.sub$nona <- profileApply(spc.sub, function(p) any(is.na(p[[var_name]])))
  spc.sub <- spc.sub[which(!spc.sub$nona),]

  # calculate the deepest top depth and shallowest bottom depth
  mindepth <- max(profileApply(spc.sub, function(p) p[,1][[hztop]]))
  maxdepth <- min(profileApply(spc.sub, estimateSoilDepth, p = pattern, name = hzdesgn))

  # we will only make interpolations that the "whole SPC supports"
  # the thought is that these 1cm slices will be further aggregated downstream
  spc.sub <- glomApply(spc.sub, function(p) c(mindepth, maxdepth), truncate = TRUE)

  # do the splines
  res <- mpspline2::mpspline(horizons(spc.sub)[c(idname(spc.sub),
                                                 horizonDepths(spc.sub),
                                                 var_name)],
                             var_name = var_name, ...)

  # concatenate results for re-insertion
  res2 <- do.call('c', lapply(profile_id(spc.sub), function(pid) {
    drange <- mindepth:maxdepth
    zero.idx <- drange == 0
    if(any(zero.idx))
      drange <- drange[-which(zero.idx)]
    return(res[[pid]]$est_1cm[drange])
    # this returns the 1cm estimate which conforms with sliced spc
    #
    # debug: prove that mass is preserved in output by returning block estimates
    # return(res[[pid]]$est_icm)
  }))

  # get the RMSE
  reserr <- do.call('c', lapply(profile_id(spc.sub), function(pid) {
    return(res[[pid]]$est_err)
  }))

  # make 1:1 with site
  reserr_iqr <- reserr[names(reserr) == "RMSE_IQR"]
  reserr <- reserr[names(reserr) == "RMSE"]

  # inspect
  #reserr_iqr
  #reserr

  # single horizon results cannot be splined, filter those out
  spc.sub <- filter(spc.sub, profile_id(spc.sub) %in% names(res))

  # adjustment for aqp::slice index logic versus glom interval logic
  if(mindepth == 0) {
    maxdepth <- maxdepth - 1
  }

  # create slices 1cm thick to insert spline result
  spc.spl <- aqp::slice(spc.sub, formula(sprintf("%s:%s ~ %s",
                                                 mindepth, maxdepth,
                                                 var_name)))

  # create new "spline_"+var_name variable
  spc.spl[[paste0("spline_",var_name)]] <- res2

  # create new "rmse_"+var_name as site level attributes
  spc.spl[[paste0("rmse_",var_name)]] <- reserr
  spc.spl[[paste0("rmse_iqr_",var_name)]] <- reserr_iqr

  # determine what profiles were removed
  removed <- profile_id(object)[!profile_id(object) %in% profile_id(spc.spl)]

  # add an attribute with removed profile IDs. there are three steps
  # that possibly remove data:
  #  - profiles removed by glomApply have no var_name data at all.
  #  - 100% coverage filtering step -- conservative filter to keep from making splines from bad data
  #  - mpspline itself will remove profiles with e.g. just one horizon
  attr(spc.spl, "removed") <- unique(removed)

  return(spc.spl)
}

###
### DEMO CODE
###
### make a combined comparison profile plot
### raw pedon data v.s. 1cm-slice + mass preserving spline
# load sample dataset
data(loafercreek, package = "soilDB")

# set all O horizons to 10 percent clay and pH 5.5
#  this isnt "necessary" but allows for interpolation to 0cm in loafercreek
loafercreek$clay[grep("O", loafercreek$hzname)] <- 10
loafercreek$phfield[grep("O", loafercreek$hzname)] <- 5.5

# use aqp wrapper function for SPC input to mpspline
res1 <- do_aqp_mpspline(loafercreek, "clay")

# NOTE: 7 profiles are dropped from loafercreek -> res
#       all of them contain no clay data at all
attr(res1, 'removed')

# inspect distribution of RMSE (one for each profile)
plot(density(res1$rmse_clay))
abline(v=quantile(res1$rmse_clay))

# set graphics parameters to minimize whitespace/leave room for legend
par(mar = c(1,1,3,1))
# make sure the removed profiles deserved to be removed
plot(filter(loafercreek, profile_id(loafercreek) %in% attr(res1,'removed')),
     color = "clay")

# NOTE: you can run this on anything numeric... but with big datasets
#  just a couple pedons missing data at a few depths will limit your range
res2 <- do_aqp_mpspline(loafercreek, "phfield")
plot(res2, color = "spline_phfield", n.legend = 5, divide.hz=FALSE, print.id=FALSE)
attr(res2, 'removed')

# more complete SPC gives broader range of depths
res3 <- do_aqp_mpspline(loafercreek[20:50], "phfield")
plot(res3[7:14,], color = "spline_phfield", n.legend = 5)
attr(res3, 'removed')

# take just a few profiles from the result for plot
res.sub <- res1[3:8,]

# get the original profiles
original <- filter(loafercreek, profile_id(loafercreek) %in% profile_id(res.sub))

# determine max depth in spline output (function of NA values)
max_d <- max(res.sub$hzdepb)

# use glom to truncate inputs to just interval matching spline output
orig.glom <- glomApply(original, function(p) {
  return(c(0, max_d)) }, truncate = TRUE)

# set new profile IDs for the spline'd profiles
profile_id(res.sub) <- paste0(profile_id(res.sub),"_mpspline")

# put spline results into variable for plotting
orig.glom$clay_combined <- orig.glom$clay
res.sub$clay_combined <- res.sub$spline_clay

# inspect
plot(res.sub[,48])

# use aqp::union to merge multiple SPCs
spc.combined <- aqp::union(list(orig.glom, res.sub))

# avoid warnings about duplicate phiid
hzidname(spc.combined) <- "hzID"

# make the comparison plot
# note with latest AQP, union will impose default sort order
#  and these will sort "correctly" due to their peiid prefix
plotSPC(spc.combined, color = "clay_combined")

# different way of looking at things
dev.off()

# set up an xy plot
plot(y=spc.combined$hzdept, x=spc.combined$spline_clay, type="n",
     xlab="Total Clay Content (%)", ylab="Depth (cm)",
     ylim=c(50,0), xlim=c(10,35))

# add line for profiles # 7 and 8
lapply(7:8, function(i) {
  lines(y=spc.combined[i,]$hzdept, x=spc.combined[i,]$clay, col=i, lwd=2)
  lines(y=spc.combined[i,]$hzdept, x=spc.combined[i,]$spline_clay, col=i, lty=2) } )

