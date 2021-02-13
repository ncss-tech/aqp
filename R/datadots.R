
#' Make a data.frame from non-standard expressions evaluated in a data environment
#'
#' @param .data A list, or object coercible to one, describing the data
#' @param ... One or more expressions (preferably named e.g. `foo = "bar"`) to evaluate in `.data`
#'
#' @return A `list` where names are expression "names" from `...` and values are the result of evaluating expressions in context of `.data`
#'
#' @examples
#' # .data_dots(data.frame(a = 1:10, b = 2:11), cc = a + b, d = cc * 2)
#
#' # data("jacobs2000", package="aqp")
#
#' # .data_dots(compositeSPC(jacobs2000), clayprop = clay / 100)
#'
.data_dots <- function(.data, ...) {
  .dots <- substitute(list(...))
  .dots <- .dots[2:length(.dots)]
  .names <- names(.dots)
  if (is.null(.names))
    .names <- paste0("expr", 1:length(.dots))
  for (i in seq_along(.dots)) {
    .data[[.names[i]]] <- eval(.dots[[i]], .data, parent.frame())
  }
  return(.data[.names])
}

