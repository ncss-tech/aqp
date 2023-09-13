library(aqp)

# example data from three official series descriptions
data("osd")



# https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
# make rlnorm simpler to parameterize
# m: arithmetic mean
# s: standard deviation
rlnorm.helper <- function(n, m, s) {
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  rlnorm(n, location, shape)
}

d <- lapply(profile_id(osd), function(i) {
  s <- data.frame(id = i, top = rlnorm.helper(50, m = 50, s = 15))
  s$bottom <- 0
  
  for(i in seq_along(s$bottom)) {
    s$bottom[i] <- s$top[i] + rlnorm.helper(1, m = 25, s = 15)
  }
  
  return(s)
})

d <- do.call('rbind', d)


# tighter margins
par(mar = c(0, 0, 0, 0))


# adjust default style
plotSPC(
  osd, 
  name.style = 'center-center', 
  cex.names = 1,
  width = 0.33,
  cex.id = 0.9,
  hz.distinctness.offset = 'hzd', 
  max.depth = 175, 
  depth.axis = FALSE, 
  hz.depths = TRUE
)

addBracket(d, offset = -0.38, tick.length = 0, lwd = 12, col = rgb(red=0, green=0, blue=1, alpha=0.03))


