library(aqp, warn.conflicts = FALSE)
library(magrittr)

par(mar=c(0,0,3,3))

data(jacobs2000)

horizons(jacobs2000)$thk_sd <- 4
horizons(jacobs2000)$bdy_sd <- 4

# seem to work in a simple case
jacobs2000[1, ] %>% 
  perturb(n = 5, boundary.attr = "bdy_sd") %>% 
  combine(jacobs2000[1,]) %>% 
  plot(color = "clay", max.depth = 200, axis.line.offset = -1)

jacobs2000[1, ] %>% 
  perturb(n = 5, thickness.attr = "thk_sd") %>% 
  combine(jacobs2000[1,]) %>% 
  plot(color = "clay", max.depth = 200, axis.line.offset = -1)

# try a large min.thickness -- seems to work
jacobs2000[1, ] %>% 
  perturb(n = 5, boundary.attr = "bdy_sd", min.thickness = 10) %>% 
  combine(jacobs2000[1,]) %>% 
  plot(color = "clay", max.depth = 200, axis.line.offset = -1)

jacobs2000[1, ] %>% 
  perturb(n = 5, thickness.attr = "thk_sd", min.thickness = 10) %>% 
  combine(jacobs2000[1,]) %>% 
  plot(color = "clay", max.depth = 200, axis.line.offset = -1)

# try max.depth = 50 -- all boundaries above 50 affected
jacobs2000[1, ] %>% 
  perturb(n = 5, boundary.attr = "bdy_sd", max.depth = 50) %>% 
  combine(jacobs2000[1,]) %>% 
  transform(thk = bottom - top) %>% 
  plot(color = "clay", name = "thk", max.depth = 200, axis.line.offset = -1)
abline(h = 50, lty = 2)

# max.depth = 50 is slightly different when perturb applied to thickness
#  note that the 3rd horizon total thickness (intersects 50) is not affected
jacobs2000[1, ] %>% 
  perturb(n = 5, thickness.attr = "thk_sd", max.depth = 50) %>% 
  combine(jacobs2000[1,]) %>% 
  transform(thk = bottom - top) %>% 
  plot(color = "clay", name = "thk", max.depth = 200, axis.line.offset = -1)
abline(h = 50, lty = 2)

# try with SPC where mindepth > 0
#  when a seed profile has shallowest depth > 0, all results have same
#  starting depth.
trunc(jacobs2000[1, ], 25, 100) %>% 
  perturb(n = 5, boundary.attr = "bdy_sd",) %>% 
  combine(jacobs2000[1,]) %>% 
  plot(color = "clay", max.depth = 200, axis.line.offset = -1)
abline(h = c(25, 100), lty = 2)

trunc(jacobs2000[1, ], 25, 100) %>% 
  perturb(n = 5, thickness.attr = "thk_sd") %>% 
  combine(jacobs2000[1,]) %>% 
  plot(color = "clay", max.depth = 200, axis.line.offset = -1)
abline(h = c(25, 100), lty = 2)
