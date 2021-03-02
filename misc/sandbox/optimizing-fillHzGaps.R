
library(aqp, warn = FALSE)
#> This is aqp 1.27

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
  hcnames <- c(idn, htb)

  h <- horizons(x)

  lead.idx <- 2:nrow(h)
  lag.idx <- 1:(nrow(h) - 1)

  # identify bad horizons
  bad.idx <- which(h[[htb[2]]][lag.idx] != h[[htb[1]]][lead.idx]
                   & h[[idn]][lag.idx] == h[[idn]][lead.idx])

  # create template data.frame
  hz.template <- h[bad.idx, ]

  if (nrow(hz.template > 0)) {
    # replace non-ID/depth column values with NA
    hz.template[, hznames[!hznames %in% hcnames]] <- NA

    # fill gaps
    hz.template[[htb[1]]] <- h[[htb[2]]][bad.idx]     # replace top with (overlying) bottom
    hz.template[[htb[2]]] <- h[[htb[1]]][bad.idx + 1] # replace bottom with (underlying) top
  }

  # flag if needed
  if (flag) {
    if (nrow(h) > 0) h[['.filledGap']] <- FALSE
    if (nrow(hz.template) > 0) hz.template[['.filledGap']] <- TRUE
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

fillHzGaps_3 <- function(x, flag = FALSE) {
  idn <- idname(x)
  hzidn <- hzidname(x)

  htb <- horizonDepths(x)

  hznames <- horizonNames(x)
  hcnames <- c(idn, htb)

  .SD <- NULL
  .N <- NULL
  .I <- NULL
  h <- data.table::as.data.table(horizons(x))

  bad.idx <- h[, .I[.SD[1:(.N - 1), 3] != .SD[2:.N, 2] &
                    .SD[1:(.N - 1), 1] == .SD[2:.N, 1]],
               .SDcols = hcnames]

  # create template data.frame
  hz.template <- h[bad.idx, ]

  if (nrow(hz.template > 0)) {
    # replace non-ID/depth column values with NA
    hz.template[, hznames[!hznames %in% hcnames]] <- NA

    # fill gaps
    hz.template[[htb[1]]] <- h[[htb[2]]][bad.idx]     # replace top with (overlying) bottom
    hz.template[[htb[2]]] <- h[[htb[1]]][bad.idx + 1] # replace bottom with (underlying) top
  }

  # flag if needed
  if (flag) {
    if (nrow(h) > 0) h[['.filledGap']] <- FALSE
    if (nrow(hz.template) > 0)  hz.template[['.filledGap']] <- TRUE
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
spc <- as.data.frame(data.table::rbindlist(lapply(1:10000, random_profile)))

# data(sp4)
# spc <- sp4
# idx <- c(6:7) # pathologically bad case for sp4
depths(spc) <- id ~ top + bottom

# introduce gaps
set.seed(12)
idx <- sample(1:nrow(spc), floor(0.01*nrow(spc)))
spc$top[idx] <- NA

# check
horizons(spc)[idx, ]

# remove problematic horizons
x <- HzDepthLogicSubset(spc, byhz = TRUE)
#> dropping horizons with invalid depth logic, see `metadata(x)$removed.horizons`

# benchmark filling gaps
bench::mark(horizons(fillHzGaps(x, flag = FALSE)),
            horizons(fillHzGaps_2(x, flag = FALSE)),
            horizons(fillHzGaps_3(x, flag = FALSE)))

#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 3 x 6
#>   expression                                  min   median `itr/sec` mem_alloc
#>   <bch:expr>                             <bch:tm> <bch:tm>     <dbl> <bch:byt>
#> 1 horizons(fillHzGaps(x, flag = TRUE))      3.28s    3.28s     0.305        NA
#> 2 horizons(fillHzGaps_2(x, flag = TRUE)) 120.73ms 133.68ms     7.53         NA
#> 3 horizons(fillHzGaps_3(x, flag = TRUE))  35.31ms  41.04ms    19.7          NA
#> # … with 1 more variable: `gc/sec` <dbl>
