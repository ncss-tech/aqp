## TODO: encode some kind of ID into the dissimilarity matrix for later use

evalGenHZ <- function(obj, genhz, vars, non.matching.code='not-used', stand=TRUE, trace=FALSE, metric='euclidean') {
  # hack to make R CMD check happy
  value <- summarize <- NULL
  
  # extract site / horizons as DF
  h <- as(obj, 'data.frame')
  
  # make an index to complete data
  no.na.idx <- which(complete.cases(h[, vars]))
  
  # test for duplicate data
  # unique IDs are based on a concatenation of variables used... digest would be safer
  h.to.test <- h[no.na.idx, c(idname(obj), vars)]
  h.to.test$id <- apply(h.to.test[, vars], 1, function(i) paste0(i, collapse = '|'))
  dupe.names <- names(which(table(h.to.test$id) > 1))
  dupe.rows <- h.to.test[which(h.to.test$id %in% dupe.names), ]
  dupe.ids <- paste0(unique(dupe.rows[[idname(obj)]]), collapse=', ')
  warning(paste0('duplicate data associated with pedons: ', dupe.ids), call. = FALSE)
    
  # compute pair-wise dissimilarities using our variables of interest
  d <- daisy(h[no.na.idx, vars], stand=stand, metric=metric)
  
  # fudge-factor in case of duplicate data (0s in the dissimilarity matrix)
  dupe.idx <- which(d < 1e-8)
  if(length(dupe.idx) > 0) {
    fudge <- min(d[which(d > 0)]) / 100
    d[dupe.idx] <- fudge
  }
  
  # perform non-metric MDS of dissimilarity matrix
  mds <- isoMDS(d, trace=trace)
  
  # compute silhouette widths after removing not-used genhz class
  sil.idx <-  which(complete.cases(h[, vars]) & h[[genhz]] != non.matching.code)
  d.sil <- daisy(h[sil.idx, vars], stand=stand)
  sil <- silhouette(as.numeric(h[[genhz]])[sil.idx], d.sil)
    
  # add new columns
  h$mds.1 <- NA
  h$mds.2 <- NA
  h$sil.width <- NA
  h$neighbor <- NA
  
  # copy values
  h$mds.1[no.na.idx] <- mds$points[, 1]
  h$mds.2[no.na.idx] <- mds$points[, 2]
  h$sil.width[sil.idx] <- sil[, 3]
  h$neighbor[sil.idx] <- levels(h[[genhz]])[sil[, 2]]
  
  # melt into long form
  m <- melt(h, id.vars=genhz, measure.vars=c(vars, 'sil.width'))
  
  # compute group-wise summaries-- note that text is returned
  m.summary <- ddply(m, c(genhz, 'variable'), function(i) {
    stats <- format(paste0(round(mean(i$value, na.rm=TRUE), 2), ' (' , sd=round(sd(i$value, na.rm=TRUE), 2), ')'), justify='right')
    return(data.frame(stats=stats))
  })
  
  fm <- paste0(genhz, ' ~ variable')
  genhz.stats <- cast(m.summary, fm, value='stats')
  
  # composite into a list
  res <- list(horizons=h[, c('mds.1', 'mds.2', 'sil.width', 'neighbor')], stats=genhz.stats, dist=d)
  return(res)
}
