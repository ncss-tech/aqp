


#' @title Fix Common Errors in Munsell Notation
#' 
#' @description
#' This function will "launder" colors in Munsell notation, fixing (and optionally flagging) the following:
#' 
#'   * colors specified with 0-chroma will be converted to neutral hue (N) and original value
#'   * for neutral colors, any chroma >0 will be set to 0
#'   * non-standard hues will be converted to NA (unless `standardHues = FALSE`)
#' 
#' Examples:
#'   * '10YR 2/0' -> 'N 2/0'
#'   * 'N 4/1 -> 'N 4/0'
#'   * '5Z 3/3' -> NA
#'   * '5YR 3/' -> NA (hues other than N must have a valid chroma)
#' 
#' See [formatMunsell()] for additional details. 
#'
#' @param m character vector of Munsell colors
#' @param verbose logical, optionally return a `data.frame` comparing modifications
#'
#' @param \dots additional arguments to [formatMunsell()]
#'
#' @returns either character vector, or when `verbose = TRUE` a `data.frame`
#'
#' @seealso [formatMunsell()]
#' @export
#'
#' @examples
#' 
#' # => 'N 2/0'
#' launderMunsell('10YR 2/0')
#' 
#' # => 'N 4/0'
#' launderMunsell('N 4/1')
#' 
#' # alternative neutral convention => 'N 4/'
#' launderMunsell('N 4/0', neutralConvention = 'empty')
#'
#' # => 'N 4/0'
#' launderMunsell('N 4/NA')
#' launderMunsell('N 4/')
#' 
#' # => 'N 4/0'
#' launderMunsell('5GY 4/0')
#' 
#' # not a standard hue => NA
#' launderMunsell('4ZR 4/6')
#' 
#' # missing chroma, not N => NA
#' launderMunsell('2.5Y 4/')
#' 
#' # invalid chroma => NA
#' launderMunsell('2.5Y 4/A')
#' 
launderMunsell <- function(m, verbose = FALSE, ...) {
  
  # split into components
  # missing chroma for N chips converted to 0
  .p <- parseMunsell(m, convertColors = FALSE)
  
  ## now handled by formatMunsell()
  # # N chips must have unspecified or 0 chroma
  # .idx <- which(.p$hue == 'N' & .p$chroma > 0)
  # 
  # if(length(.idx) > 0) {
  #   .p$chroma[.idx] <- 0
  # }
  
  # any hue other than N with chroma of 0 => N
  .idx <- which(.p$hue != 'N' & .p$chroma == 0)
  
  if(length(.idx) > 0) {
    .p$hue[.idx] <- 'N'
  }
  
  # combine back into standard notation
  # following conventions
  .res <- formatMunsell(hue = .p$hue, value = .p$value, chroma = .p$chroma, ...)
  
  # optionally combine into a data.frame for inspection
  if(verbose) {
    .res <- data.frame(
      original = m, 
      fixed = .res, 
      modified = !m == .res
    )
  }
  
  return(.res)
  
}
