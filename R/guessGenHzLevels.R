# x: SoilProfileCollection
# h: horizon-level attribute, typically a GHL
# result: list of levels and median depths
guessGenHzLevels <- function(x, hz='genhz') {
  tb <- horizonDepths(x)
  h <- horizons(x)
  # compute horizon mid-point
  m <- (h[[tb[1]]] + h[[tb[2]]]) / 2
  # median mid-point is probably a good indicator of depth-wise ordering
  m.med <- tapply(m, h[[hz]], median, na.rm=TRUE)
  # sort and return
  s <- sort(m.med)
  return(list(levels=names(s), median.depths=s))
}
