
library(data.table)
library(aqp)
library(magrittr)
#
# # build the data
# system.time(bigrandom <- lapply(1:1000001, random_profile))
# # user  system elapsed
# # 933.54    1.95  939.94
#
# # save for later
#
load(file = "bigrandom.Rda")

# combine (better than rbind.fill for this [conforming DFs] case)
#system.time(bigrandom <- rbindlist(bigrandom))
# user  system elapsed
# 7.34    0.06    8.17

# load from before
# load("bigrandom.Rda")

# promote to SPC (numeric IDs requiring much reordering...)
#system.time(depths(bigrandom) <- id ~ top + bottom)
# user  system elapsed
# 82.17   10.53   99.89

#save(bigrandom, file = "bigrandom.Rda")

# get first horizon
key(bigrandom@horizons)
system.time(foo <- bigrandom[, 1])

# original code
# user  system elapsed
# 21.30    5.27   30.25
#
# new code 
# user  system elapsed
# 12.602   0.193  11.521
#
# slower on windows/work machine 
#   user  system elapsed 
# 22.40    9.97   40.42 

bigrandom <- aqp:::.enforce_df_class(bigrandom, "data.table")
key(bigrandom@horizons)

system.time(foo <- bigrandom[, 1])

# same with key (using by=idname(x) internally)
# user  system elapsed
# 12.377   0.034  11.179

# make sure this works and ids are sorted and right profiles are there
explainPlotSPC(bigrandom[1:10,], color = "p3")
explainPlotSPC(foo[1:10,1], color = "p3")

# mutate + group_by (on first horizons)
#  - calculate first horizon thickness (implicitly becomes site level prop)
#  - calculate whether 1) thickness is > 15cm
#  - calculate whether 2) random var p1 is greater than zero
#  - calculate combined group 1+2 membership (all false, one true, all true)
system.time({
  foo <- transform(foo,
                thickness = bottom - top,
                group1 = thickness > 15,
                group2 = p1 > 0,
                group = group1 + group2) %>%
         groupSPC(group)
})
# user  system elapsed
# 0.09    0.00    0.09

# basic group_by-mediated summaries
system.time(print(summarizeSPC(foo, mean(thickness))))

# converting group to a factor
# group mean(thickness)
# 1:     0        9.998191
# 2:     1       17.494020
# 3:     2       23.009373
#
# old code
# user  system elapsed
# 9.05    0.12    9.12
#
#  new code
# user  system elapsed
# 5.118   0.100   5.095
#
# binary split methods based on a grouping var
# performance approx equal; proportional to number of records in group / number of groups
# split is generally as fast or faster and good for ngrp > 2; gives list output
microbenchmark::microbenchmark( { foo1 <- subset(foo, group > 0) },
                                { foo2 <- subset(foo, group == 0) },
                                { foo3 <- aqp::split(foo, "group") },
                                { foo4 <- aqp::split(foo, "group1") },
                                times = 1)
# old code
# Unit: seconds
#                                 expr       min        lq      mean    median        uq       max
# {     foo1 <- filter(foo, group > 0) } 13.740669 13.740669 13.740669 13.740669 13.740669 13.740669
# {     foo2 <- filter(foo, group == 0) }  6.709405  6.709405  6.709405  6.709405  6.709405  6.709405
# {     foo3 <- aqp::split(foo, "group") }  9.309963  9.309963  9.309963  9.309963  9.309963  9.309963
# {     foo4 <- aqp::split(foo, "group1") }  9.540006  9.540006  9.540006  9.540006  9.540006  9.540006

# new code
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# {     foo1 <- filter(foo, group > 0) } 5.915227 5.915227 5.915227 5.915227 5.915227 5.915227     1
# {     foo2 <- filter(foo, group == 0) } 3.477218 3.477218 3.477218 3.477218 3.477218 3.477218     1
# {     foo3 <- aqp::split(foo, "group") } 4.927283 4.927283 4.927283 4.927283 4.927283 4.927283     1
# {     foo4 <- aqp::split(foo, "group1") } 5.422631 5.422631 5.422631 5.422631 5.422631 5.422631     1

# single bracket subsetting (big random SPC)
# in general, unconstrained j-index subsetting is slowest (because it impacts sitelevel too)
microbenchmark::microbenchmark( { foo1 <- bigrandom[1,] },
                                { foo2 <- bigrandom[1,1] },
                                { foo3 <- bigrandom[,1] },
                                { foo4 <- bigrandom[,1:10] },
                                times = 1)
# old code
# Unit: milliseconds -- first horizon from 1 million profiles in ~30 seconds
# expr        min         lq       mean     median         uq        max neval
# {     foo1 <- bigrandom[1, ] }   233.4691   233.4691   233.4691   233.4691   233.4691   233.4691     1
# {     foo2 <- bigrandom[1, 1] }  1821.2622  1821.2622  1821.2622  1821.2622  1821.2622  1821.2622     1
# {     foo3 <- bigrandom[, 1] } 29891.3303 29891.3303 29891.3303 29891.3303 29891.3303 29891.3303     1
# {     foo4 <- bigrandom[, 1:10] } 42630.6587 42630.6587 42630.6587 42630.6587 42630.6587 42630.6587     1

# # new code -- first horizon from 1 million profiles in ~12 seconds
# Unit: milliseconds
# expr        min         lq       mean     median         uq        max neval
# {     foo1 <- bigrandom[1, ] }   131.2488   131.2488   131.2488   131.2488   131.2488   131.2488     1
# {     foo2 <- bigrandom[1, 1] }   117.9077   117.9077   117.9077   117.9077   117.9077   117.9077     1
# {     foo3 <- bigrandom[, 1] } 11386.4235 11386.4235 11386.4235 11386.4235 11386.4235 11386.4235     1
# {     foo4 <- bigrandom[, 1:10] } 19704.7755 19704.7755 19704.7755 19704.7755 19704.7755 19704.7755     1

# single bracket subsetting (on a SPC comprised entirely of 1 horizon profiles)
# in general, unconstrained j-index subsetting is slowest (because it impacts sitelevel too)
microbenchmark::microbenchmark( { foo1 <- foo[1,] },
                                { foo2 <- foo[1,1] },
                                { foo3 <- foo[,1] },
                                { foo4 <- foo[,1:10] },
                                times = 1)
# old code
# Unit: milliseconds
#                     expr        min         lq       mean     median         uq        max neval
# {     foo1 <- foo[1, ] }   149.5651   149.5651   149.5651   149.5651   149.5651   149.5651     1
# {     foo2 <- foo[1, 1] }   644.4674   644.4674   644.4674   644.4674   644.4674   644.4674     1
# {     foo3 <- foo[, 1] } 28986.2621 28986.2621 28986.2621 28986.2621 28986.2621 28986.2621     1
# {     foo4 <- foo[, 1:10] } 28264.6826 28264.6826 28264.6826 28264.6826 28264.6826 28264.6826     1
#
# new code
# # Unit: milliseconds
# expr        min         lq       mean     median         uq        max neval
# {     foo1 <- foo[1, ] }   49.92942   49.92942   49.92942   49.92942   49.92942   49.92942     1
# {     foo2 <- foo[1, 1] }   50.81557   50.81557   50.81557   50.81557   50.81557   50.81557     1
# {     foo3 <- foo[, 1] } 9846.62283 9846.62283 9846.62283 9846.62283 9846.62283 9846.62283     1
# {     foo4 <- foo[, 1:10] } 9964.02291 9964.02291 9964.02291 9964.02291 9964.02291 9964.02291     1

library(profvis)
profvis::profvis( { foo3 <- bigrandom[,4] })
