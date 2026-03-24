
# setup a new environment to store error messages, etc.
aqp.env <- new.env(hash = TRUE, parent = parent.frame())

# register options for later use
.onLoad <- function(libname, pkgname) {
  options(.aqp.show.n.cols = 10)
  options(.aqp.verbose = FALSE)
}


# 2020-05-30: make data.table, tbl_df and data.frame slots "co-exist"
# see: https://stackoverflow.com/questions/35642191/tbl-df-with-s4-object-slots
if(requireNamespace("data.table", quietly = TRUE))
  setOldClass(c("data.table", "data.frame"))

if(requireNamespace("tibble", quietly = TRUE))
  setOldClass(c("tbl_df", "tbl", "data.frame"))
