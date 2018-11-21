
## TODO: this doesn't take into account non-default figure geometry
# annotate profile plots with group labels, usually below
profileGroupLabels <- function(x0, x1, labels, y0=100, y1=98, label.offset=2, label.cex=0.75) {
  
  # sanity check: start / stop / label lengths should be equal
  if(! all.equal(length(x0), length(x1), length(labels)) )
    stop('start positions, stop positions, and number of labels must be equal', call. = FALSE)
  
  # pre-compute some elements
  n.groups <- length(x0)
  label.centers <- (x0 + x1) / 2
  
  # add group base lines
  segments(x0=x0, x1=x1, y0=y0, y1=y0)
  # add arrows to first / last group members
  arrows(x0=c(x0, x1), x1=c(x0, x1), y0=c(y0, y0), y1=y1, length=0.1)
  
  # annotate with group names
  text(x=label.centers, y=y0 + label.offset, labels=labels, cex=label.cex)
}

## TODO: labeling is not very helpful
## TODO: figure out intellegent recycling of arguments
## TODO: no mechanism for merged legends
plotMultipleSPC <- function(spc.list, group.labels, args=rep(list(NA), times=length(spc.list)), arrow.offset=2, bracket.base.depth=95, ...) {
  
  # compute group stats
  n.groups <- length(spc.list)
  spc.lengths <- sapply(spc.list, length)
  n.pedons <- sum(spc.lengths)
  group.starts <- c(1, 1 + cumsum(spc.lengths[-n.groups]))
  group.ends <- cumsum(spc.lengths)
  
  # get depths + offset to start / end profiles
  yy <- unlist(sapply(spc.list, function(i) profileApply(i, max)))
  tick.heights <- yy[c(group.starts, group.ends)] + arrow.offset
  
  # setup plot with first SPC in list
  do.call(plotSPC, c(x=spc.list[[1]], n=n.pedons, na.omit(args[[1]]), ...))
  
  # iterate over remaining SPC objs
  if(n.groups > 1) {
    for(i in 2:n.groups) {
      this.obj <- spc.list[[i]]
      this.args <- na.omit(args[[i]])
      do.call(plotSPC, c(x=this.obj, x.idx.offset=group.ends[i-1], add=TRUE, plot.depth.axis=FALSE, this.args))
    }
  }
  
  # annotate with group brackets
  profileGroupLabels(x0=group.starts, x1=group.ends, labels=group.labels, y0=bracket.base.depth, y1=tick.heights) 
}




