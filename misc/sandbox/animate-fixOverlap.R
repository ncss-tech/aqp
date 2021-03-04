library(aqp)
library(av)

x <- c(1, 1.2, 3.4, 3.5, 4.1, 5, 6, 7.8, 8, 9, 10, 12)

d <- lapply(letters[1:length(x)], random_profile, SPC = TRUE, method = 'LPP')
d <- combine(d)

set.seed(10101)
x.fixed <- fixOverlap(x, thresh = 0.7, trace = TRUE)

# idx <- which(x.fixed$log == '+')
idx <- seq_along(x.fixed$stats)
d.max <- profileApply(d, max)

m <- function() {
  lapply(idx, function(i) {
    
    par(mar = c(0, 0, 0, 0))
    
    plotSPC(d, color = 'p1', relative.pos = x.fixed$states[i, ], plot.depth.axis = FALSE, name.style = 'center-center', hz.depths = TRUE)
    
    txt <- sprintf("iteration: %s\ncost: %s\nlog: %s", i, round(x.fixed$stats[i] + x.fixed$ssd[i], 3), x.fixed$log[i])
    mtext(txt, side = 3, line = -3, at = 1, adj = 0, cex = 0.85)
    
    axis(side = 1, line = -3, at = seq_along(x))
    segments(x0 = x.fixed$states[i, ], y0 = max(d), x1 = x.fixed$states[i, ], y1 = d.max, lty = 3)
    
  })
  
}


video_file <- file.path(tempdir(), 'output.mp4')
av::av_capture_graphics(m(), video_file, 1200, 650, framerate = 30, res = 90, vfilter = 'framerate=fps=30')
av::av_media_info(video_file)
utils::browseURL(video_file)


# tracePlot(x, x.fixed)

