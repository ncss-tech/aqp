# AGB (2020/05/27)
#'
#' Eliminate duplicate instances of profile IDs in a list of SoilProfileCollections
#'
#'  @description Experimental function to "clean" list input where duplicates exist (that would otherwise prevent \code{pbindlist}). Useful for queries that may have overlapping instances of the same data, for instance a list of SoilProfileCollections where each list element contains profiles gathered from a set of (potentially overlapping) extents.
#'
#' @param l A list of SoilProfileCollections.
#'
#' @return A list of SoilProfileCollections, with duplicate profile IDs removed.
#'
#' @author Andrew G. Brown
#'
#' @export lunique
#'
#' @examples
#'
#' data(sp5)
#'
#' # EXAMPLE #1 -- resolving overlap
#'
#' # 6 profiles in four sets, and 5,6,7 are missing
#' input <- lapply(list(c(1,3,4), c(2,2,3), NA, c(8,9,1)), function(idx) {
#'       if(!all(is.na(idx)))
#'        sp5[idx,]
#' })
#'
#' output <- lunique(input)
#'
#' # 6 profiles are in final SPC; 5,6,7 are missing
#' match(profile_id(pbindlist(output)), profile_id(sp5))
#'
#' # EXAMPLE #2 -- exact duplicates
#'
#' # deliberately duplicate an SPC
#' sp5_2 <- sp5
#' res <- lunique(list(sp5, sp5_2))
#'
#' # the number of profiles in first element is equal to number in sp5
#' length(res[[1]]) == length(sp5)
#'
#' # second list element contains NA b/c all uniques are in #1
#' res[[2]]
#'
lunique <- function(l) {

  # calculate profile IDs for each SPC element in l
  l.pid <- lapply(l, function(x) {
    if(!inherits(x, 'SoilProfileCollection')) {
      return(NA)
    }
    profile_id(x)
  })

  # keep track of all-NA and length (number of profiles per set)
  l.na <- as.logical(unlist(lapply(l.pid, function(x) all(is.na(x)))))
  l.n <- lapply(l.pid, length)

  # make data frame of profile ID and input list index
  df <- data.frame(pid = do.call('c', l.pid),
                   idx = do.call('c', lapply(1:length(l.n),
                                             function(n) rep(n, l.n[n]))))

  # squash non-uniques in combined data.frame, based on profile ID
  df <- df[order(df$pid),]
  d.out <- do.call('rbind', lapply(split(df, df$pid), function(d) {
    # note diagnostics on d can be helpful to ID type of non-uniqueness
    return(d[1,])
  }))

  # split back into "original" list elements -- sans empty ones
  d.split <- split(d.out, d.out$idx)
  l.out <- lapply(1:length(l.na), function(k) {
    if(!l.na[k]) {
      needle <- l.pid[[k]]
      haystack <- d.split[[as.character(k)]]
      ids <- which(needle %in% haystack$pid)
      if(length(ids) == 0)
        return(NA)
      return(l[[k]][ids, ])
    } else {
      return(NA)
    }
  })

  # put NAs back in that were lost during concatenation
  return(l.out)
}
