#' @title Transform a SPC (by profile) with a set of expressions
#' @name mutate_profile
#' @aliases mutate_profile,SoilProfileCollection-method
#' @description \code{mutate_profile()} is a function used for transforming SoilProfileCollections. Each expression is applied to site or horizon level attributes of individual profiles. This distinguishes this function from \code{mutate}, which is applied to all values in a collection, regardless of which profile they came from.
#' @param object A SoilProfileCollection
#' @param ... A set of comma-delimited R expressions that resolve to a transformation to be applied to a single profile e.g \code{mutate_profile(hzdept = max(hzdept) - hzdept)}
#' @param horizon_level logical. If `TRUE` results of expressions are added to the SoilProfileCollection's horizon slot, if `FALSE` the results are added to the site slot. If `NULL` (default) the results are stored in the site or horizon slot based on the number of rows in each slot compared to the length of the result calculated from the _first_ and _last_ profile in the collection.
#' 
#' @details If the length an expression's result matches the number of horizons, the result is stored as a horizon-level variable. If the result has length 1, it is stored as a site-level variable. In the ambiguous case where the first and last profile have only _one_ horizon, the results are stored in the horizon slot by default. To force results into site slot use `horizon_level = FALSE`.
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#'
#' @rdname mutate_profile
#' @export mutate_profile
# if (!isGeneric("mutate_profile"))
setGeneric("mutate_profile", function(object, ..., horizon_level = NULL) standardGeneric("mutate_profile"))

setMethod("mutate_profile", signature(object = "SoilProfileCollection"), function(object, ..., horizon_level = NULL) {

    # capture expression(s) at function
    .dots <- substitute(list(...))
    .dots <- .dots[2:length(.dots)]
    .names <- names(.dots)

    if (is.null(.names)) {
      .names <- as.character(.dots)
    }
    
    # iterate over expressions left to right
    for (i in 1:length(.dots)) {
      if (is.null(horizon_level) || !is.logical(horizon_level)){
        horizon_level <- FALSE
        
        # decide whether we are adding/modifying a site or horizon level variable so
        #  that degenerate cases do not create identical columns in site and horizon table or get put in unexpected slot
        #  
        #  2021-10-29: updated to use first and last profile, and allowing user override via argument
        res_eval1 <- .data_dots(compositeSPC(object[1,]), eval(.dots[[i]]))[[1]]
        res_eval2 <- .data_dots(compositeSPC(object[nrow(object),]), eval(.dots[[i]]))[[1]]
        if (length(res_eval1) == nrow(object[1,]) && length(res_eval2) == nrow(object[nrow(object),])) {
           horizon_level <- TRUE
        }
      }
      
      # then iterate over profiles within expression, frameify results
      res <- profileApply(object, function(o) {

        # create composite object to facilitate eval_tidy
        .data <- compositeSPC(o)
        res_eval <- .data_dots(.data, eval(.dots[[i]]))[[1]]

        if (horizon_level) {
          horizons(o)[[.names[i]]] <- res_eval
        } else {
          site(o)[[.names[i]]] <- res_eval
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
})
