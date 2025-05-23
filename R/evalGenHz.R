## TODO: encode some kind of ID into the dissimilarity matrix for later use
#' Evaluate Generalized Horizon Labels
#'
#' Data-driven evaluation of generalized horizon labels using nMDS and
#' silhouette width.
#'
#' Classic multidimensional scaling is performed via [stats::cmdscale()].
#' The input distance matrix is generated by [cluster::daisy()] using
#' (complete cases of) horizon-level attributes from `obj` as named in
#' `vars`.
#'
#' Silhouette widths are computed via [cluster::silhouette()]. The input
#' distance matrix is generated by [cluster::daisy()] using (complete cases
#' of) horizon-level attributes from `obj` as named in `vars`. Note
#' that observations with genhz labels specified in `non.matching.code`
#' are removed filtered before calculation of the distance matrix.
#'
#' @param obj a `SoilProfileCollection` object
#' 
#' @param genhz name of horizon-level attribute containing generalized horizon labels
#' 
#' @param vars character vector of horizon-level attributes to include in the evaluation
#' 
#' @param non.matching.code code used to represent horizons not assigned a generalized horizon label
#' 
#' @param stand standardize variables before computing distance matrix, passed to [cluster::daisy()]
#' 
#' @param metric distance metric, passed to [cluster::daisy()]
#' 
#' @return a list is returned containing: 
#'   * horizons: `c('mds.1', mds.2', 'sil.width', 'neighbor')` 
#'   * stats: mean and standard deviation `vars`, computed by generalized horizon label
#'   * dist: the distance matrix as passed to [stats::cmdscale()]
#'   
#' @author D.E. Beaudette
#' 
#' @seealso [get.ml.hz()]
#' 
#' @keywords manip
#' 
#' @export
evalGenHZ <- function(obj, genhz = GHL(obj, required = TRUE), vars, non.matching.code = 'not-used', stand = TRUE, metric = 'euclidean') {

  # hack to make R CMD check happy
  value <- summarize <- NULL

  # extract site / horizons as DF
  h <- as(obj, 'data.frame')

  # genhz may have its own levels set, but if not a factor, make one
  if (!is.factor(h[[genhz]]))
    h[[genhz]] <- factor(h[[genhz]])

  # make an index to complete data
  no.na.idx <- which(complete.cases(h[, vars, drop = FALSE]))

  ## TODO: all of vars should be numeric or convertable to numeric
  # numeric.test <- sapply(vars, function(i) is.numeric(h[[i]]))

  # test for duplicate data
  # unique IDs are based on a concatenation of variables used... digest would be safer
  h.to.test <- h[no.na.idx, c(idname(obj), vars)]
  h.to.test$id <- apply(h.to.test[, vars, drop = FALSE], 1, function(i) paste0(i, collapse = '|'))
  dupe.names <- names(which(table(h.to.test$id) > 1))
  dupe.rows <- h.to.test[which(h.to.test$id %in% dupe.names), ]
  dupe.ids <- paste0(unique(dupe.rows[[idname(obj)]]), collapse = ', ')
  if (dupe.ids != "")
    warning(paste0('duplicate data associated with pedons: ', dupe.ids), call. = FALSE)

  # compute pair-wise dissimilarities using our variables of interest
  d <- cluster::daisy(h[no.na.idx, vars, drop = FALSE], stand = stand, metric = metric)

  # fudge-factor in case of duplicate data (0s in the dissimilarity matrix)
  dupe.idx <- which(d < 1e-8)
  if (length(dupe.idx) > 0) {
    fudge <- min(d[which(d > 0)]) / 100
    d[dupe.idx] <- fudge
  }

  ## TODO: allow user-defined ordination function
  # perform metric MDS of dissimilarity matrix
  mds <- stats::cmdscale(d, k = 2)

  # compute silhouette widths after removing not-used genhz class
  sil.idx <-  which(complete.cases(h[, vars, drop = FALSE]) & h[[genhz]] != non.matching.code)
  d.sil <- cluster::daisy(h[sil.idx, vars, drop = FALSE], stand=stand)
  sil <- cluster::silhouette(as.numeric(h[[genhz]])[sil.idx], d.sil)

  # add new columns
  h$mds.1 <- NA
  h$mds.2 <- NA
  h$sil.width <- NA
  h$neighbor <- NA

  ## TODO: access results from user-defined ordination function
  # copy values
  h$mds.1[no.na.idx] <- mds[, 1]
  h$mds.2[no.na.idx] <- mds[, 2]
  h$sil.width[sil.idx] <- sil[, 3]
  h$neighbor[sil.idx] <- levels(h[[genhz]])[sil[, 2]]

  ## TODO: enforce / check above
  ## important note: all 'vars' should be numeric


  # convert wide -> long form
  # using data.table::melt
  # suppressing warnings related to mixture of int / numeric
  m <- suppressWarnings(
    data.table::melt(
      data.table::as.data.table(h),
      id.vars = genhz,
      measure.vars = c(vars, 'sil.width')
    )
  )

  # leave as data.table for aggregation
  # compute group-wise summaries
  m.summary <- m[, list(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE)), by = c((genhz), 'variable')]

  # format text
  m.summary$stats <- sprintf(
    "%s (%s)",
    round(m.summary$mean, 2),
    round(m.summary$sd, 2)
  )

  # using data.table::dcast
  fm <- paste0(genhz, ' ~ variable')
  genhz.stats <- dcast(data = m.summary, formula = fm, value.var = 'stats')

  # convert back to data.frame
  genhz.stats <- as.data.frame(genhz.stats)

  # composite into a list
  res <- list(horizons = h[, c('mds.1', 'mds.2', 'sil.width', 'neighbor')],
              stats = genhz.stats, dist = d)
  return(res)
}
