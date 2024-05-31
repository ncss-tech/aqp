library(aqp)

data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'

# test effect of potential sorting alpha vs. numeric
# profile_id(sp4) <- as.character(1:length(sp4))

profile_id(sp4) <- sprintf("%0.3d", 1:length(sp4))

# profile_id(sp4) <- letters[1:length(sp4)]

.top <- 10
.bottom <- 35

d <- dice(sp4, (.top):(.bottom - 1) ~ .)
g <- glom(sp4, z1 = .top, z2 = .bottom)
gt <- glom(sp4, z1 = .top, z2 = .bottom, truncate = TRUE)
st <- hz_segment(sp4, intervals = c(.top, .bottom))
s <- hz_segment(sp4, intervals = c(.top, .bottom), trim = FALSE)


# normalize IDs
profile_id(d) <- sprintf("%s\nD", profile_id(d))
profile_id(g) <- sprintf("%s\nG", profile_id(g))
profile_id(gt) <- sprintf("%s\nGT", profile_id(gt))
profile_id(s) <- sprintf("%s\nS", profile_id(s))
profile_id(st) <- sprintf("%s\nST", profile_id(st))
profile_id(sp4) <- sprintf("%s\n", profile_id(sp4))

x <- combine(sp4, d, g, gt, s, st)

par(mar = c(0, 0, 3, 0))

plotSPC(x, color = 'CEC_7', name.style = 'center-center', width = 0.4, id.style = 'top', col.palette = hcl.colors(25, palette = 'viridis'), depth.axis = list(line = -5))

abline(h = c(.top, .bottom), lwd = 2, lty = 3, col = 'red')



z <- duplicate(sp4, times = 100)
library(microbenchmark)

m <- microbenchmark(
  dice = dice(z, (.top):(.bottom - 1) ~ .), 
  glom = glom(z, z1 = .top, z2 = .bottom, truncate = TRUE), 
  hz_segment = hz_segment(sp4, intervals = c(.top, .bottom), trim = TRUE), 
  times = 10
)

m

