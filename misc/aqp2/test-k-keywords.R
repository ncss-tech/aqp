# DEMO: base non-standard eval of keyword in ... "k-index": SPC[i, j, ...]
# version 1: add support for .LAST
# version 2: add support for .FIRST, .HZID and multiple special keywords

library(aqp, warn = FALSE)

data(sp4)
depths(sp4) <- id ~ top + bottom

# .LAST
sp4[, , .LAST]
sp4[, , .HZID]
sp4[, , .LAST, .HZID]
sp4[, , .FIRST, .HZID] # this just sets j <- 1
sp4[, 1, , .HZID]
sp4[, 1000, .FIRST, .HZID] # this sets j <- 1; ignores j input if given
sp4[, 1000, .LAST, .HZID] # this ignores j input if given

# horizon index of 2nd horizon in each profile
sp4[5:10, 2, .HZID]

# this is more realistic, perhaps:
fivetoten <- sp4[5:10,] # or some more complicated subset()

# third horizon ID, index to horizon data.frame
horizons(fivetoten)[fivetoten[,3,,.HZID],]

# use it to implement #199
getLastHorizonID_v1 <- function(x) {
  res <-  hzID(x[, , .LAST])
  names(res) <- profile_id(x)
  return(res)
}

# ~3x more efficient using j-index shortcut
getLastHorizonID_v2 <- function(x) {
  res <-  hzID(x)[x[, , .LAST, .HZID]]
  names(res) <- profile_id(x)
  return(res)
}

bench::mark(getLastHorizonID_v1(sp4),
            getLastHorizonID_v2(sp4))

# bigger <- data.table::rbindlist(lapply(1:1000, random_profile))
# depths(bigger) <- id ~ top + bottom
# bench::mark(x <- getLastHorizonID_v1(bigger),
#             x <- getLastHorizonID_v2(bigger))
