


#' @title Fix Common Errors in Munsell Notation
#' 
#' @description
#' This function will "launder" colors in Munsell notation, fixing (and optionally flagging) the following:
#' 
#'   * colors specified with 0-chroma will be converted to neutral hue (N) and original value
#'   * for neutral colors, any chroma >0 will be set to 0
#' 
#' Examples:
#'   * '10YR 2/0' -> 'N 2/0'
#'   * 'N 4/1 -> 'N 4/0'
#' 
#'
#' @param m character vector of Munsell colors
#' @param verbose logical, optionally return a `data.frame` comparing modifications
#'
#' @returns either character vector, or when `verbose = TRUE` a `data.frame`
#' @export
#'
#' @examples
#' 
#' # will be converted to 'N 4/0'
#' launderMunsell('5G 4/0')
#' 
launderMunsell <- function(m, verbose = FALSE) {
  
  # split into components
  # missing chroma for N chips converted to 0
  .p <- parseMunsell(m, convertColors = FALSE)
  
  # N chips must have unspecified or 0 chroma
  .idx <- which(.p$hue == 'N' & .p$chroma > 0)
  
  if(length(.idx) >0) {
    .p$chroma[.idx] <- 0
  }
  
  # any chip with chroma == 0 -> N chip
  .idx <- which(.p$hue != 'N' & .p$chroma == 0)
  
  if(length(.idx) >0) {
    .p$hue[.idx] <- 'N'
  }
  
  # combine back into standard notation
  # missing chroma become 0
  .res <- sprintf("%s %s/%s", .p$hue, .p$value, .p$chroma)
  
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
