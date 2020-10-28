#' @title Transform a SPC (by profile) with a set of expressions
#' @name mutate_profile
#' @aliases mutate_profile,SoilProfileCollection-method
#' @description \code{mutate_profile()} is a function used for transforming SoilProfileCollections. Each expression is applied to site or horizon level attributes of individual profiles. This distinguishes this function from \code{mutate}, which is applied to pooled values (across individuals) in a collection/group.
#' @param object A SoilProfileCollection
#' @param ... A set of comma-delimited R expressions that resolve to a transformation to be applied to a single profile e.g \code{mutate_profile(hzdept = max(hzdept) - hzdept)}
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname mutate_profile
#' @export mutate_profile
if (!isGeneric("mutate_profile"))
  setGeneric("mutate_profile", function(object, ...) standardGeneric("mutate_profile"))

setMethod("mutate_profile", signature(object = "SoilProfileCollection"),
          function(object, ...) {
  if(requireNamespace("rlang", quietly = TRUE)) {

    # capture expression(s) at function
    x <- rlang::enquos(..., .named = TRUE)

    # TODO: group_by would operate above profile?
    #       is it safe to assume operations, in general, would
    #       be on the profile basis, and then aggregated by group?

    # TODO: evaluate expressions individually, to handle dependence?
    # currently, iterates through profiles evaluating each expression on each profile
    # need to iterate over expressions, then through profiles for each expression
    # then, mutate_profile(spc, c = b + a, d = c) would not error with 'c' not found

    # iterate over expressions left to right
    for(n in names(x)) {

      # then iterate over profiles within expression, frameify results
      res <- profileApply(object, function(o) {

        # create composite object to facilitate eval_tidy
        data <- compositeSPC(o)
        o[[n]] <- rlang::eval_tidy(x[[n]], data)

        return(list(st=site(o), hz=horizons(o)))
      }, simplify = FALSE)

      # make a big data.frame
      if (requireNamespace("data.table")) {
        .site <- .as.data.frame.aqp(data.table::rbindlist(lapply(res, function(r) { r$st }), fill = TRUE), aqp_df_class(object))
        .hz <- .as.data.frame.aqp(data.table::rbindlist(lapply(res, function(r) { r$hz }), fill = TRUE), aqp_df_class(object))
      } else {
        .site <- do.call('rbind', lapply(res, function(r) { r$st }))
        .hz  <- do.call('rbind',  lapply(res, function(r) { r$hz }))
      }

      # never set rownames
      rownames(.site) <- NULL
      rownames(.hz) <- NULL

      slot(object, 'site') <- .site
      slot(object, 'horizons') <- .hz
    }

    return(object)
  } else {
    stop("package 'rlang' is required for mutate_profile", .call=FALSE)
  }
})
