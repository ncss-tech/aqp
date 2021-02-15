spc_j_DT <- function(x, i, j) {

  #  this is already live in AQP for data.table SPCs -- is it worth enabling globally?

  h <- data.table::as.data.table(horizons(x))

  # local vars to make R CMD check happy
  .N <- NULL
  .I <- NULL
  V1 <- NULL

  # data.table can do this much more efficiently
  # if (requireNamespace("data.table", quietly = TRUE)) {
  idn <- idname(x)

  # by list @horizons idname (essentially iterating over profiles)
  bylist <- list(h[[idn]])
  names(bylist) <- idn

  # figured out the data.table way to do this
  #  not using := or . anymore

  # determine j indices to KEEP
  j.idx <- h[, .I[1:.N %in% j], by = bylist]$V1

  # determine which site indices to keep
  # in case all horizons are removed, remove sites too
  if (length(j.idx) == 0) {
    i.idx <- numeric(0)
  } else {
    # determine which profile IDs KEEP
    pids <- h[, .I[any(1:.N %in% j)][1], by = bylist]
    i.idx <- pids[, .I[!is.na(V1)]]
  }
  return(list(i.idx, j.idx))
}

# benchmark faster replacement of j subsetting of horizon data

spc_j_DT_2 <- function(x, i, j) {

  #  this is already live in AQP for data.table SPCs
  # question: is it worth enabling globally?

  h <- data.table::as.data.table(horizons(x))

  # local vars to make R CMD check happy
  .N <- NULL
  .I <- NULL
  V1 <- NULL

  idn <- idname(x)

  # by list @horizons idname (essentially iterating over profiles)
  bylist <- list(h[[idn]])
  names(bylist) <- idn

  # figured out the data.table way to do this
  #  not using := or . anymore

  # determine j indices to KEEP
  j.idx <- h[, .I[1:.N %in% j], by = bylist]$V1

  # determine which site indices to keep
  # in case all horizons are removed, remove sites too
  if (length(j.idx) == 0) {
    i.idx <- numeric(0)
  } else {
    # determine which profile IDs KEEP
    i.idx <- which(profile_id(x) %in% unique(h[j.idx,][[idn]]))
  }
  return(list(i.idx, j.idx))
}

spc_j_base <- function(x, i, j) {
    h <- horizons(x)
    # retain a base R way of doing things (plenty fast with SPCs up to ~100k or so)
    j.res <- as.list(aggregate(
        h[[hzidname(x)]],
        by = list(h[[idname(x)]]),
        FUN = function(hh) {
          list(1:length(hh) %in% j)
        },
        drop = FALSE
      )$x)

    ##  https://github.com/ncss-tech/aqp/issues/89
    # fix #89, where i with no matching j e.g. @site data returned
    i.idx <- which(as.logical(lapply(j.res, function(jr) { any(jr) })))
    j.idx <-  which(do.call('c', j.res))

    return(list(i.idx, j.idx))
}

library(aqp)

data(sp4)
depths(sp4) <- id ~ top + bottom

# base outperforms old DT with 10 profiles, but new solution slightly better
microbenchmark::microbenchmark(spc_j_base(sp4, 1:10, 2),
                               spc_j_DT(sp4, 1:10, 2),
                               spc_j_DT_2(sp4, 1:10, 2))

# whether the SPC is a "data.table SPC" ahead of time does not affect overhead much
sp4_DT <- data.table::as.data.table(horizons(sp4))
depths(sp4_DT) <- id ~ top + bottom

bench::mark(spc_j_base(sp4_DT, 1:10, 2:3),
            spc_j_DT(sp4_DT, 1:10, 2:3),
            spc_j_DT_2(sp4_DT, 1:10, 2:3))

# basically neck-and-neck at 1000 profiles; closing the gap of base over DT from ~50% to about ~5%
# the new solution is 200% faster than both old ones
thousand <- as.data.frame(data.table::rbindlist(lapply(1:1000, random_profile)))
depths(thousand) <- id ~ top + bottom

bench::mark(spc_j_base(thousand, 1:10, 2:3),
            spc_j_DT(thousand, 1:10, 2:3),
            spc_j_DT_2(thousand, 1:10, 2:3))

# ten thousand profiles, we see a benefit from DT (old: ~20% faster than base; new 300% faster)
tenthousand <- as.data.frame(data.table::rbindlist(lapply(1:10000, random_profile)))
depths(tenthousand) <- id ~ top + bottom

bench::mark(spc_j_base(tenthousand, 1:10, 2:3),
            spc_j_DT(tenthousand, 1:10, 2:3),
            spc_j_DT_2(tenthousand, 1:10, 2:3))

# hundred thousand profiles, we see a similar benefit from DT -- 2 to 3 times faster
hundredthousand <- as.data.frame(data.table::rbindlist(lapply(1:100000, random_profile)))
depths(hundredthousand) <- id ~ top + bottom

bench::mark(spc_j_base(hundredthousand, 1:10, 2:3),
            spc_j_DT(hundredthousand, 1:10, 2:3),
            spc_j_DT_2(hundredthousand, 1:10, 2:3))

# # million profiles, we see a benefit from DT
# million <- as.data.frame(data.table::rbindlist(lapply(1:1000000, random_profile)))
# depths(million) <- id ~ top + bottom
#
# bench::mark(
#   spc_j_base(million, 1:10, 2:3),
#   spc_j_DT(million, 1:10, 2:3),
#   spc_j_DT_2(hundredthousand, 1:10, 2:3)
# )
