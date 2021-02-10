library(aqp)

# get sample data from @jskovlin; USFS d.phorizon .rda file
load("E:/workspace/Ofixer/d.phorizon.Rda")

# apply SPC-style character ID + top depth sort
d.phorizon <- d.phorizon[order(as.character(d.phorizon$peiidref),
                               d.phorizon$hzdept),]

# check logic of sorted data frame
res <- checkHzDepthLogic(d.phorizon, c("hzdept", "hzdepb"), "peiidref")

# find the bad depthLogic (either NA depths, or bottom < top)
bad.peiids <- subset(res, depthLogic)[['peiidref']]

# "good" set -- leave them alone for now (still may have other errors)
good.d.phorizon <- subset(d.phorizon, !(peiidref %in% bad.peiids))

# bad set -- we will try to fix O horizons
bad.d.phorizon <- subset(d.phorizon, peiidref %in% bad.peiids)

# add 1cm to R or Cr horizons that have NA hzdepb
bad.r.idx <- which(with(bad.d.phorizon, grepl("R|Cr", hzname) & is.na(hzdepb)))
bad.d.phorizon$hzdepb[bad.r.idx] <- bad.d.phorizon$hzdept[bad.r.idx] + 1

# add 1cm to O horizons that have equal hzdept and hzdepb
bad.thk.idx <- which(with(bad.d.phorizon, grepl("O", hzname) & (hzdept == hzdepb)))
bad.d.phorizon$hzdepb[bad.thk.idx] <- bad.d.phorizon$hzdept[bad.thk.idx] + 1

# remove any rows with both depths NA

# inspect these / fix elsewhere
bad.d.phorizon_toinspect <- subset(bad.d.phorizon, is.na(hzdept) & is.na(hzdepb))
which(!is.na(bad.d.phorizon_toinspect$texture))


bad.peiids2 <- subset(checkHzDepthLogic(bad.d.phorizon, c("hzdept", "hzdepb"), "peiidref"), 
                      depthLogic)[['peiidref']]

# "good" set -- leave them alone for now (still may have other errors)
good.d.phorizon <- subset(d.phorizon, !(peiidref %in% bad.peiids2))

# bad set -- we will try to fix O horizons
bad.d.phorizon <- subset(d.phorizon, peiidref %in% bad.peiids2)

# these we can try to fix (remove all NA rows)
bad.d.phorizon <- subset(bad.d.phorizon, !is.na(hzdept) & !is.na(hzdepb))

# look for the specific logic error 
#  (O horizon with bottom depth shallower than top depth)
bad.o.idx <- which(with(bad.d.phorizon, (grepl("O", hzname) | (is.na(hzname) & seqnum == 1)) & hzdepb < hzdept))
bad.o.peiids <- bad.d.phorizon[bad.o.idx, 'peiidref']

# make negative
bad.d.phorizon[bad.o.idx, c("hzdept","hzdepb")] <- -bad.d.phorizon[bad.o.idx, c("hzdept","hzdepb")]

# re-order using SPC-style ID+top depth sorting (again!)
bad.d.phorizon <- bad.d.phorizon[order(as.character(bad.d.phorizon$peiidref),
                                       bad.d.phorizon$hzdept),]

# calculate thickness using ordered horizons
bad.d.phorizon$thk <- bad.d.phorizon$hzdepb - bad.d.phorizon$hzdept
bad.d.phorizon$thk[is.na(bad.d.phorizon$thk)] <- 0

# convert to data.table
bad.d.phorizon_after <- data.table::as.data.table(bad.d.phorizon)

# cumulative sums of thickness to make new top/bottom depths
bad.d.phorizon_after <- bad.d.phorizon_after[, list(hzdept = c(min(abs(hzdept)), cumsum(thk[1:(.N - 1)])),
                                                    hzdepb = min(abs(hzdept)) + cumsum(thk)), 
                                             by = peiidref]

# res <- daff::diff_data(bad.d.phorizon[,c("peiidref","hzdept","hzdepb")], 
#                        as.data.frame(bad.d.phorizon_after)[,c("peiidref","hzdept","hzdepb")])
# daff::render_diff(res)

stillbad <- checkHzDepthLogic(bad.d.phorizon_after, c("hzdept", "hzdepb"), "peiidref")

# View(subset(bad.d.phorizon_after, peiidref %in% stillbad$peiidref[!stillbad$valid]))

bad.d.phorizon$hzdept <- bad.d.phorizon_after$hzdept
bad.d.phorizon$hzdepb <- bad.d.phorizon_after$hzdepb
bad.d.phorizon$thk <- NULL

final <- rbind(good.d.phorizon, bad.d.phorizon)

finalres <- checkHzDepthLogic(final, c("hzdept", "hzdepb"), "peiidref")

# all results pass depthLogic check
sum(finalres$depthLogic)

subset(d.phorizon, peiidref %in% finalres$peiidref[finalres$depthLogic])

# no pedon IDs are missing 
d.phorizon$peiidref[!d.phorizon$peiidref %in% finalres$peiidref]

# 178 still have some sort of logic error (it seems)
sum(!finalres$valid)

subset(final, peiidref == 103)
