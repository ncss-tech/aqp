.callinsharpshootR <- function(fname, ...) {
  if (!requireNamespace("sharpshootR", warn.conflicts = FALSE, quietly = TRUE))
    stop("this function has moved! please install the `sharpshootR` package")
  get(fname, envir = as.environment("package:sharpshootR"))
}

# test data
# library(aqp)
# data(jacobs2000, package = 'aqp')
# p <- jacobs2000[1]
#
# attr <- 'clay' # clay contents
# p$texcl <- with(horizons(p), ssc_to_texcl(clay, sand))

#' Soil Taxonomy Heuristics (moved to sharpshootR package)
#'
#' @param ... arguments to functions deprecated from aqp namespace
#' @aliases argillic.clay.increase.depth crit.clay.argillic estimatePSCS get.increase.depths get.increase.matrix getArgillicBounds getCambicBounds hasDarkColors mollic.thickness.requirement
#' @export
#' @rdname ST-heuristics
getArgillicBounds <- function(...) {
   # .Deprecated("sharpshootR::getArgillicBounds")
  .callinsharpshootR("getArgillicBounds")(...)
}

estimatePSCS <- function(...) {
  # .Deprecated("sharpshootR::estimatePSCS")
  .callinsharpshootR("estimatePSCS")(...)
}

getCambicBounds <- function(...) {
  # .Deprecated("sharpshootR::getCambicBounds")
  .callinsharpshootR("getCambicBounds")(...)
}

mollic.thickness.requirement <- function(...) {
  # .Deprecated("sharpshootR::mollic.thickness.requirement")
  .callinsharpshootR("mollic.thickness.requirement")(...)
}

get.increase.depths <- function(...) {
  # .Deprecated("sharpshootR::get.increase.depths")
  .callinsharpshootR("get.increase.depths")(...)
}

argillic.clay.increase.depth <- function(...) {
  # .Deprecated("sharpshootR::argillic.clay.increase.depth")
  .callinsharpshootR("argillic.clay.increase.depth")(...)
}

crit.clay.argillic <- function(...) {
  # .Deprecated("sharpshootR::crit.clay.argillic")
  .callinsharpshootR("crit.clay.argillic")(...)
}

getArgillicBounds <- function(...) {
  # .Deprecated("sharpshootR::getArgillicBounds")
  .callinsharpshootR("getArgillicBounds")(...)
}

hasDarkColors <- function(...) {
  # .Deprecated('sharpshootR::hasDarkColors")
  .callinsharpshootR("hasDarkColors")(...)
}

