library(aqp)


# example SPC from data.frame
data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'


# test effect of potential sorting alpha vs. numeric
# all seem to work


# profile_id(sp4) <- as.character(1:length(sp4))

profile_id(sp4) <- sprintf("%0.2d", 1:length(sp4))

# profile_id(sp4) <- letters[1:length(sp4)]


testIt <- function(spc, top, bottom) {
  
  # keep old ID
  spc$.oldID <- profile_id(spc)
  
  # various approaches
  
  # dice() fills missing depth intervals with NA
  # default when given a formula
  .fm <- as.formula(sprintf("%s:%s ~ .", top, bottom - 1))
  d <- dice(spc, fm = .fm)
  
  # force filling missing depth intervals with NA
  g <- glom(spc, z1 = top, z2 = bottom, fill = TRUE)
  gt <- glom(spc, z1 = top, z2 = bottom, truncate = TRUE, fill = TRUE)
  
  # single NA horizon, with NA depths
  st <- hz_segment(spc, intervals = c(top, bottom))
  s <- hz_segment(spc, intervals = c(top, bottom), trim = FALSE)
  
  
  # normalize profile IDs
  # so that all can be combined / viewed together in a single SPC
  profile_id(d) <- sprintf("%s\nD", profile_id(d))
  profile_id(g) <- sprintf("%s\nG", profile_id(g))
  profile_id(gt) <- sprintf("%s\nGT", profile_id(gt))
  profile_id(s) <- sprintf("%s\nS", profile_id(s))
  profile_id(st) <- sprintf("%s\nST", profile_id(st))
  profile_id(spc) <- sprintf("%s\n", profile_id(spc))
  
  x <- combine(spc, d, g, gt, s, st)
  
  par(mar = c(0, 0, 3, 0))
  
  plotSPC(x, color = 'CEC_7', name.style = 'center-center', width = 0.4, id.style = 'top', col.palette = hcl.colors(25, palette = 'viridis'), depth.axis = list(line = -5))
  
  segments(x0 = 0, x1 = length(x) + 1, y0 = c(top, bottom), y1 = c(top, bottom), lwd = 2, lty = 3, col = 'red')
  
  invisible(x)
  
}

testIt(sp4, top = 0, bottom = 25)
# check for all-NA horizons, and equal number of sites / original ID
table(a$.oldID)


a <- testIt(sp4, top = 15, bottom = 35)
# check for all-NA horizons, and equal number of sites / original ID
table(a$.oldID)


a <- testIt(sp4, top = 20, bottom = 21)
# check for all-NA horizons, and equal number of sites / original ID
table(a$.oldID)


a <- testIt(sp4, top = 50, bottom = 60)
# check for all-NA horizons, and equal number of sites / original ID
table(a$.oldID)




# 
# # rough estimate of function run time, on a lager collection
# z <- duplicate(sp4, times = 100)
# library(microbenchmark)
# 
# m <- microbenchmark(
#   dice = dice(z, (.top):(.bottom - 1) ~ .), 
#   glom = glom(z, z1 = .top, z2 = .bottom, truncate = TRUE), 
#   hz_segment = hz_segment(sp4, intervals = c(.top, .bottom), trim = TRUE), 
#   times = 10
# )
# 
# m


