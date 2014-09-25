evalGenHZ <- function(obj, genhz, vars, non.matching.code='not-used', stand=TRUE, trace=FALSE) {
  # extract horizons
  h <- horizons(obj)
  
  # make an index to complete data
  no.na.idx <- which(complete.cases(h[, vars]))
    
  # compute pair-wise dissimilarities using our variables of interest
  d <- daisy(h[no.na.idx, vars], stand=stand)
  
  # apply fudge-factor in case of duplicate data (0s in the dissimilarity matrix)
  fudge <- min(d) / 100
  d <- d + fudge
  
  # perform non-metric MDS of dissimilarity matrix
  mds <- isoMDS(d, trace=trace)
  
  # compute silhouette widths after removing not-used genhz class
  idx <-  which(complete.cases(h[, vars]) & h[[genhz]] != non.matching.code)
  d <- daisy(h[idx, vars], stand=stand)
  sil <- silhouette(as.numeric(h[[genhz]])[idx], d)
    
  # add new columns
  h$mds.1 <- NA
  h$mds.2 <- NA
  h$sil.width <- NA
  h$neighbor <- NA
  
  # copy values
  h$mds.1[no.na.idx] <- mds$points[, 1]
  h$mds.2[no.na.idx] <- mds$points[, 2]
  h$sil.width[idx] <- sil[, 3]
  h$neighbor[idx] <- levels(h[[genhz]])[sil[, 2]]
  
  # compute group-wise summaries
  m <- melt(h, id.vars=genhz, measure.vars=c(vars, 'sil.width'))
  m.summary <- ddply(m, c(genhz, 'variable'), summarize, 
                     stats=paste0(round(mean(value, na.rm=TRUE), 2), ' (' , sd=round(sd(value, na.rm=TRUE), 2), ')')
                     )
  fm <- paste0(genhz, ' ~ variable')
  genhz.stats <- cast(m.summary, fm, value='stats')
  
  # composite into a list
  res <- list(horizons=h[, c('mds.1', 'mds.2', 'sil.width', 'neighbor')], stats=genhz.stats)
  return(res)
}
