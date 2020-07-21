
# this function is not (yet) used anywhere

# goal is to be able to take any SPC and convert to X internals

.enforce_df_class <- function(object, use_class) {
  
  # check class and convert as needed
  object@horizons <- .as.data.frame.aqp(object@horizons, use_class)
  object@site <- .as.data.frame.aqp(object@site, use_class)
  object@diagnostic <- .as.data.frame.aqp(object@diagnostic, use_class)
  object@restrictions <- .as.data.frame.aqp(object@restrictions, use_class)

  if (use_class == "data.table") {
    
    # namespace requirement for setting data.table keys
    if (!requireNamespace("data.table"))
      stop("package `data.table is required", call. = FALSE)
    
    # set keys on data.table objects
    data.table::setkeyv(object@horizons, c(idname(object), horizonDepths(object)[1]), verbose = FALSE)
    
    data.table::setkeyv(object@site, idname(object), verbose = FALSE)

    if (idname(object) %in% names(object@diagnostic))
      data.table::setkeyv(object@diagnostic, idname(object), verbose = FALSE)
    
    if (idname(object) %in% names(object@restrictions))
      data.table::setkeyv(object@restrictions, idname(object), verbose = FALSE)
  }

  return(object)
}
