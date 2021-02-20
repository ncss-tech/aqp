library(aqp, warn.conflicts = FALSE)
library(testthat)

data(jacobs2000)

# use glom to create some fake missing data (REMOVE 50-100cm zone)
input.spc <- glom(jacobs2000[4,], 50, 100, 
                  truncate = TRUE, invert = TRUE)

##################################
### .:. # 5 cannot segment a single profile if the interval contains missing data (gaps)
##################################
# Error: the 50:100 zone is missing; and only one profile
(output.spc <- segment(input.spc, 50:100))

# Error: even with data above and below 50/150 -- single profile
(output.spc <- segment(input.spc, 25:150))

# Error: just 1 cm of missing data in single profile case breaks it
(output.spc <- segment(input.spc, 0:51))

# Works
segment(input.spc, 0:50)

# Add another profile
input.spc <- combine(input.spc, jacobs2000[7,])

# Works
output.spc <- segment(input.spc, 50:100)


##################################
### .:. #6 output ID order does not match input order or input profiles
##################################

# _NEEDS_ FIX: not only does it have same IDs... it appears to change the order of inputs
(all(profile_id(input.spc) == profile_id(output.spc))) # FALSE

##################################
### .:. #7 does not provide new IDs for re-sampled profiles
##################################

# Error: Cannot combine because the transformed SPC has the same ID 
#  (this is a feature not a bug, resampled horizons != original data)
(show.spc <- combine(input.spc, output.spc))

# Inspect the default segment ID output -- this is wrong
par(mfrow=c(1,2),mar=c(0,0,2,1))
plot(input.spc, max.depth=200, color="matrix_color", plot.depth.axis=FALSE)
plot(output.spc, max.depth=200, color="matrix_color", name=NA)

# # segment should assign new profile IDs automatically
# # the correct IDs are something like thisthis:
output.spc2 <- output.spc

fix.idx <- match(profile_id(output.spc), profile_id(input.spc))
profile_id(output.spc2) <- paste0(profile_id(input.spc)[fix.idx], "segmented")

par(mfrow=c(1,2),mar=c(0,0,2,1))
plot(input.spc, max.depth=200, color="matrix_color", plot.depth.axis=FALSE)
plot(output.spc2, max.depth=200, color="matrix_color", name=NA)

# # note: that I am deliberately accounting for reversed order w/ match / `fix.idx`
# 
#         this is not a solution, but shows why ID integrity is important
#         numeric (site and horizon/slice) indices are used pretty frequently so
#         if they are shifting around in between calls to functions that can lead
#         to unexpected problems inside and outside the SoilProfileCollection


# Suggestions:
# 
# Under normal circumstances, it is impossible to make an SPC like the segment output from the base data
# 
#   This is because of the forced ordering that `depths<-` does when starting with the horizon data
#   
#   Temporary solutions:
#    - rebuilding the SPC "fixes" the apparent issue in the algorithm
#      - again this is not an actual fix -- the re-ordering of profiles should be addressed

output.spc3 <- rebuildSPC(output.spc)

par(mfrow=c(1,2),mar=c(0,0,2,1))
plot(input.spc, max.depth=200, color="matrix_color")
plot(output.spc3, max.depth=200, color="matrix_color", divide.hz=FALSE, name=NA)

#   Long term solution:     
#    - In long term we should not calling `rebuildSPC` inside a function unless there is some intractable error.
#      - we do provide one instance of this in `pbindlist` merging non-conformal (different source/columns) SPCs
#          
#    - `segment` should build a SPC using conventional means (`depths<-`)
#     - by building the SPC with depths, you avoid the problem by writing your algorithm on its own
#     - let the SoilProfileCollection object handle doing the proper ordering.
#       - This is why `replaceHorizons(object) <- h` is dangerous. 
#      
#   Opinion:
#    - I know that I advised of this `replaceHorizons` issue some time ago. The old "replace" behavior of `horizons()` should have never existed, at least not without the proper validity checks in place, which was not the case until very recently (early/mid summer 2020). I have second thoughts about exporting `replaceHorizons` at all because of problems exactly like this. 



