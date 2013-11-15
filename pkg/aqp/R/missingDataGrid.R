# s: soil profile collection object
# s.fm: slicing formula, including variables requested for missing data test
# cols: vector of colors palette
missingDataGrid <- function(s, s.fm, cols=NULL) {
  
  # default color scheme
  if(is.null(cols))
    cols <- rev(brewer.pal(8, 'Spectral'))
  
  # make color pallete and define number of cuts
  cols.palette <- colorRampPalette(cols)
  ncuts <- 20
  
#   # get original horizon bottom depths as a list, in order of our profiles
#   obd <- profileApply(s, simplify=FALSE, FUN=function(i) {
#     unique(unlist(horizons(i)[, horizonDepths(i)]))
#   })
  
  # slice according to rules
  ss <- slice(s, s.fm)
  
  # get sliced horizon depth names
  hd <- horizonDepths(ss)
  
  # extract horizons from sliced data
  h <- horizons(ss)
  
  # get slice mid-points
  h$mid <- (h[[hd[1]]] + h[[hd[2]]]) / 2
  
  # construct levelplot formula using horizon top boundaries
  f <- as.formula(paste('.pctMissing',  ' ~ ', 'factor(', idname(ss), ') * mid', sep=''))
  
  # ylab adjustments
  ylab <- paste('Depth ', '(', depth_units(ss), ')', sep='')
  
  # depth-range adjustments
  ylim <- c(max(h$mid) + 5, -5)
  
  # plot missing data fraction
  levelplot(f, data=h, ylim=ylim, col.regions=cols.palette(ncuts), cuts=ncuts-1, ylab=ylab, xlab='', scales=list(x=list(rot=90), y=list(tick.number=10)), main='Missing Data Fraction', panel=function(...) {
    panel.levelplot(...)
    panel.abline(v=1:(length(ss)+1)-0.5)
    panel.grid(h=-1, v=FALSE, lty=2, col=grey(0.25))
#     for(i in 1:length(obd)) {
#       panel.segments(i-0.5, obd[[i]], i+0.5, obd[[i]])
#     }
  })
  
}
