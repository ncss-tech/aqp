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
    for (n in names(x)) {
      horizon_level <- FALSE
      # decide whether we are adding/modifying a site or horizon level variable so
      #  that degenerate cases do not create identical columns in site and horizon table
      res_eval <- rlang::eval_tidy(x[[n]], compositeSPC(object[1,]))
      if (length(res_eval) == nrow(object[1,]))
         horizon_level <- TRUE
         
      # then iterate over profiles within expression, frameify results
      res <- profileApply(object, function(o) {

        # create composite object to facilitate eval_tidy
        data <- compositeSPC(o)
        res_eval <- rlang::eval_tidy(x[[n]], data)

        if (horizon_level) {
          horizons(o)[[n]] <- res_eval
        } else {
          site(o)[[n]] <- res_eval
        }
        
        return(list(st = site(o), hz = horizons(o)))
      }, simplify = FALSE)

      # make a big data.frame
      if (requireNamespace("data.table")) {
        if (!horizon_level)
         .site <- .as.data.frame.aqp(data.table::rbindlist(lapply(res, function(r) { r$st })), aqp_df_class(object))
        else 
         .hz <- .as.data.frame.aqp(data.table::rbindlist(lapply(res, function(r) { r$hz })), aqp_df_class(object))
      } else {
        if (!horizon_level)
         .site <- do.call('rbind', lapply(res, function(r) { r$st }))
        else 
         .hz  <- do.call('rbind',  lapply(res, function(r) { r$hz }))
      }

      if (!horizon_level) {
        rownames(.site) <- NULL
        slot(object, 'site') <- .site
      } else {
        rownames(.hz) <- NULL
        slot(object, 'horizons') <- .hz
      }
    }

    return(object)
  } else {
    stop("package 'rlang' is required for mutate_profile", .call=FALSE)
  }
})
