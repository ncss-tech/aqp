
# setup a new environment to store error messages, etc.
aqp.env <- new.env(hash=TRUE, parent = parent.frame())

# register options for later use
.onLoad <- function(libname, pkgname) {
  options(.aqp.show.n.cols=10)
  
}
