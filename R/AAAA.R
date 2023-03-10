
# setup a new environment to store error messages, etc.
aqp.env <- new.env(hash = TRUE, parent = parent.frame())

# register options for later use
.onLoad <- function(libname, pkgname) {
  options(.aqp.show.n.cols = 10)

}

# no longer needed since it is imported
# # 2020-07-10: allows for data.table @Suggests without importing
# # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
# .datatable.aware <-  TRUE

# 2020-05-30: make data.table, tbl_df and data.frame slots "co-exist"
# see: https://stackoverflow.com/questions/35642191/tbl-df-with-s4-object-slots
if(requireNamespace("data.table", quietly = TRUE))
  setOldClass(c("data.table", "data.frame"))

if(requireNamespace("tibble", quietly = TRUE))
  setOldClass(c("tbl_df", "tbl", "data.frame"))
