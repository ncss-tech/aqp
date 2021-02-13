# I made a reprex for the `subset()` clone that fails when used programmatically using base NSE
# the solution is `parent.frame(n=2)`
#
# ``` r
# .data_dots <- function(.data, ...) {
#   .dots <- substitute(list(...))
#   .dots <- .dots[2:length(.dots)]
#   .names <- names(.dots)
#   if (is.null(.names))
#     .names <- paste0("expr", 1:length(.dots))
#   for (i in seq_along(.dots)) {
#     .data[[.names[i]]] <- eval(.dots[[i]], .data, parent.frame())
#   }
#   return(.data[.names])
# }
#
# my_subset_bad <- function(.data, ...) {
#   f <- .data_dots(.data, ...)
#   idx <- which(rowSums(f) == length(f))
#   .data[idx, , drop = FALSE]
# }
#
# testdata <- data.frame(a = 1:3, b = 4:6)
#
# # works
# my_subset(testdata, b == 5)
# #>   a b
# #> 2 2 5
#
# # doesn't work
# lapply(1:3, function(i) my_subset_bad(testdata, a == i))
# #> Error in eval(.dots[[i]], .data, parent.frame()): object 'i' not found
# ```
#
# In this case the variable `i` is not visible when my substituted expression `a == i` from `.data_dots(...)` is `eval`-ed inside
# the `for()` loop. This is because of `parent.frame()` being called from inside the loop.
#

#'
#' An example of base non-standard evaluation
#'
#' Note that this is expected to be used directly, not programmatically.
#'
#' @param .data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
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

# basic call
.data_dots(data.frame(a = 1:10, b = 2), c = a * b)

# programmatic
lapply(1:10, (function(i)
  .data_dots(data.frame(
    a = 1:10, b = 2
  ), c = a * b * i)))

# make a custom NSE function with {base}
my_subset <- function(.data, ...) {
  .dots <- substitute(list(...))
  .dots <- .dots[2:length(.dots)]
  .names <- names(.dots)
  if (is.null(.names))
    .names <- paste0("expr", 1:length(.dots))
  f <- list()
  for (i in seq_along(.dots)) {
    f[[.names[i]]] <- eval(.dots[[i]], .data, parent.frame()) # parent.frame(1) appropriate here
  }
  idx <- which(rowSums(as.data.frame(f)) == length(f))
  .data[idx, , drop = FALSE]
}

testdata <- data.frame(a = 1:3, b = 4:6)

# works
my_subset(testdata, b == 5)
lapply(1:3, function(i) my_subset(testdata, a == i))
lapply(1:3, function(i) (function(j) my_subset(testdata, a == j))(i))

base::subset(testdata, b == 5)
lapply(1:3, function(i) base::subset(testdata, a == i))
lapply(1:3, function(i) (function(j) base::subset(testdata, a == j))(i))

dplyr::filter(testdata, b == 5)
lapply(1:3, function(i) dplyr::filter(testdata, a == i))
lapply(1:3, function(i) (function(j) dplyr::filter(testdata, a == j))(i))

# inspect the call stack

get_global_env <- function(...) {
  res <- environment()
  i <- 0 # NB: 1 offset
  while (!identical(res, globalenv())) {
    i <- i + 1
    res <- parent.frame(i)
  }
  return(i)
}

get_global_env() # level 1: "interactive"

(function() get_global_env())() # level 2: in a function
lapply(1, get_global_env) # level 2: call with lapply
vapply(1, get_global_env, double(1)) # level 2: call with vapply

sapply(1, get_global_env) # level 3: call with sapply
lapply(1, function(i) get_global_env()) # level 3: lapply a function that calls the function

lapply(1, function(i) (function() get_global_env())()) # level 4: lapply a function that calls a function that calls the function

# try with the real thing

library(aqp)

data(sp4)
depths(sp4) <- id ~ top + bottom

# works!
lapply(1:10, function(i) subset(sp4, profile_id(sp4) %in% id[i]))

# works!
lapply(1:10, function(i) (function(j) subset(sp4, profile_id(sp4) %in% id[j]))(i))

# aqp::subset needs parent.frame(n = 2) in bare call to base::eval -- WHY?
# passing R CMD CHECK, but not feeling solid about it
