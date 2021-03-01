library(aqp, warn = FALSE)

# original code, slightly modified relative to master
fillHzGaps <- function(x, flag = FALSE) {
  
  idn <- idname(x)
  hzidn <- hzidname(x)
  htb <- horizonDepths(x)
  hznames <- horizonNames(x)
  
  ids.top.bottom.idx <- match(c(idn, hzidn, htb), hznames)
  
  h <- horizons(x)
  
  hs <- split(h, h[[idn]])
  
  h.filled <- lapply(hs, function(i) {
    n <- nrow(i)
    
    s <- 1:(n-1)
    
    .top <- i[[htb[1]]]
    .bottom <- i[[htb[2]]]
    idx <- which(.bottom[s] != .top[s + 1])
    
    if(length(idx) > 0) {
      gap.top <- .bottom[idx]
      gap.bottom <- .top[idx + 1]
      
      hz.template <- i[1, ids.top.bottom.idx]
      hz.template[[htb[1]]] <- gap.top
      hz.template[[htb[2]]] <- gap.bottom
      hz.template[[hzidn]] <- NA
      res <- data.table::rbindlist(list(i, hz.template), fill = TRUE)
      res <- as.data.frame(res)
      return(res)
    } else {
      return(i)
    }
    
  })
  
  h.filled <- do.call('rbind', h.filled)
  o <- order(h.filled[[idn]], h.filled[[htb[1]]])
  h.filled <- h.filled[o, ]
  idx <- which(is.na(h.filled[[hzidn]]))
  
  if(length(idx) > 0) {
    m <- max(as.numeric(h[[hzidn]]), na.rm = TRUE)
    s <- seq(
      from = m + 1,
      to = m + length(idx),
      by = 1
    )
    
    if(flag) {
      h.filled[['.filledGap']] <- FALSE
      h.filled[['.filledGap']][idx] <- TRUE
    }
    
    h.filled[[hzidn]][idx] <- as.character(s)
  }
  
  # note: this is the right place to deal with hzid
  h.filled$hzID <- as.character(1:nrow(h.filled))
  replaceHorizons(x) <- aqp:::.as.data.frame.aqp(h.filled, aqp_df_class(x))
  hzidname(x) <- "hzID"
  
  return(x)
}

# new code
fillHzGaps_2 <- function(x, flag = FALSE) {
  idn <- idname(x)
  hzidn <- hzidname(x)
  
  htb <- horizonDepths(x)
  
  hznames <- horizonNames(x)
  hcnames <- c(idn, hzidn, htb)
  
  h <- horizons(x)
  
  lead.idx <- 2:nrow(h)
  lag.idx <- 1:(nrow(h) - 1)
  
  # identify bad horizons
  bad.idx <- which(h[[htb[2]]][lag.idx] != h[[htb[1]]][lead.idx]
                   & h[[idn]][lag.idx] == h[[idn]][lead.idx])
  
  # create template data.frame
  hz.template <- h[bad.idx, ]
  
  # replace non-ID/depth column values with NA
  hz.template[, hznames[!hznames %in% hcnames]] <- NA
  
  # fill gaps
  hz.template[[htb[1]]] <- h[[htb[2]]][bad.idx]     # replace top with (overlying) bottom
  hz.template[[htb[2]]] <- h[[htb[1]]][bad.idx + 1] # replace bottom with (underlying) top
  
  # flag if needed
  if (flag) {
    h[['.filledGap']] <- FALSE
    hz.template[['.filledGap']] <- TRUE
  }  
  
  # combine original data with filled data
  res <- rbind(h, hz.template)
  
  # ID + top depth sort
  res <- res[order(res[[idn]], res[[htb[1]]]),]
  
  # re-calculate unique hzID (note: AFTER reorder)
  res$hzID <- as.character(1:nrow(res))
  
  # replace horizons (use df class in object x)
  replaceHorizons(x) <- aqp:::.as.data.frame.aqp(res, aqp_df_class(x))
  
  # use the autocalculated hzID (in case user had e.g. phiid, chiid set)
  hzidname(x) <- "hzID"
  
  return(x)
}

# create sample dataset
data(sp4)
depths(sp4) <- id ~ top + bottom

# introduce gaps
idx <- c(2, 8, 12)
sp4$top[idx] <- NA

# check
horizons(sp4)[idx, ]

# remove problematic horizons
x <- HzDepthLogicSubset(sp4, byhz = TRUE)

# benchmark filling gaps
bench::mark(horizons(fillHzGaps(x, flag = TRUE)), 
            horizons(fillHzGaps_2(x, flag = TRUE)), 
            min_iterations = 100)
