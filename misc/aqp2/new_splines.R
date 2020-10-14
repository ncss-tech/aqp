# load packages
library(aqp) # for SoilProfileCollection object + wrangling code
library(soilDB) # sample data
library(mpspline2) # generic mass-preserving/equal area spline implementation

### make a combined comparison profile plot
### raw pedon data v.s. 1cm-slice + mass preserving spline
# load sample dataset
data(loafercreek, package = "soilDB")

# set all O horizons to 10 percent clay and pH 5.5
#  this isnt "necessary" but allows for interpolation to 0cm in loafercreek
loafercreek$clay[grep("O", loafercreek$hzname)] <- 10
loafercreek$phfield[grep("O", loafercreek$hzname)] <- 5.5

# use aqp wrapper function for SPC input to mpspline
res1 <- spc2mpspline(loafercreek, "clay")

# NOTE: 7 profiles are dropped from loafercreek -> res
#       all of them contain no clay data at all
attr(res1, 'removed')

# inspect distribution of RMSE (one for each profile)
plot(density(res1$clay_rmse))
abline(v = quantile(res1$clay_rmse))

# set graphics parameters to minimize whitespace/leave room for legend
par(mar = c(1,1,3,1))

# make sure the removed profiles deserved to be removed (they are all NA clay)
plot(filter(loafercreek, profile_id(loafercreek) %in% attr(res1,'removed')), color = "clay")

# NOTE: you can run this on anything numeric... but with big datasets
#  just a couple pedons missing data at a few depths will limit your range
res2 <- spc2mpspline(loafercreek, "phfield")
plot(res2, color = "phfield_spline", n.legend = 5, divide.hz = FALSE, print.id = FALSE)
attr(res2, 'removed')

# more complete SPC gives broader range of depths
res3 <- spc2mpspline(loafercreek[20:50], "phfield")
plot(res3[7:14,], color = "phfield_spline", n.legend = 5)
attr(res3, 'removed')

# take just a few profiles from the result for plot
res.sub <- res1[3:8,]

# get the original profiles
original <- filter(loafercreek, profile_id(loafercreek) %in% profile_id(res.sub))

# determine max depth in spline output (function of NA values)
max_d <- max(res.sub$hzdepb)

# use glom to truncate inputs to just interval matching spline output
orig.glom <- trunc(original, 0, max_d)

# duplicate profiles, create a harmonized thematic variable from spline and original variables
foo <- harmonize(res.sub, list(clay = c(spline = "clay_spline", original = "clay")))

# combine with original input geometry truncated to same interval
spc.combined <- combine(orig.glom, foo)

# inspect profiles
plot(spc.combined, color = "clay")

par(mar = c(5.1,4.1,4.1,2.1))

# set up an xy line plot to compare two profiles
plot(y = spc.combined$hzdept, x = spc.combined$clay, type = "n",
     xlab = "Total Clay Content (%)", ylab = "Depth (cm)",
     ylim = c(50,0), xlim = c(10,40))

# we now have multiple profiles per siteiid
spc.list <- split(spc.combined, f = "siteiid")

# add line for profiles # 7 and 8
color <- 7:8 # also use colors 7 and 8 (coincidentally nice)
sub.spc.list <- tail(spc.list, 2)
lapply(1:2, function(i) {
   p1 <- sub.spc.list[[i]][2,] # raw (sliced) clay data
   p2 <- sub.spc.list[[i]][3,] # splined clay data
   lines(y = p1$hzdept, x = p1$clay, col = color[i], lwd = 2)
   lines(y = p2$hzdept, x = p2$clay, col = color[i], lty = 2) 
  } )

