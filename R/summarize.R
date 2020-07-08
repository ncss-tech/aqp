#' @title Perform summaries on groups (from \code{group_by}) and create new site or horizon level attributes
#' @name summarize
#'
#' @aliases summarize,SoilProfileCollection-method
#'
#' @description \code{summarize()} is a function used for summarizing SoilProfileCollections. Specify the groups using the group_by verb, and then (named) expressions to evaluate on each group. The result is a data.frame with one row per categorical level in the grouping variable and one column for each summary variable.
#'
#' @param object A SoilProfileCollection
#' @param ... A set of (named) comma-delimited R expressions that resolve to a summary value. e.g \code{groupmean = mean(clay, na.rm = TRUE)}
#'
#' @return A data.frame with one row per level in the grouping variable, and one column for each summary
#'
#' @author Andrew G. Brown
#'
#' @rdname summarize
#' @export summarize
#'
if (!isGeneric("summarize"))
  setGeneric("summarize", function(object, ...) 
    standardGeneric("summarize"))

setMethod("summarize", signature(object = "SoilProfileCollection"),
          function(object, ...) {
  if(requireNamespace("rlang")) {
    # TODO: safe setter and accessor methods for grouping variable
    group.by.col <- object@metadata$aqp_group_by

    if (length(group.by.col) == 0)
      group.by.col <- idname(object)

    groups <- levels(factor(as.character(object[[group.by.col]])))

    # capture expression(s) at function
    x <- rlang::enquos(..., .named = TRUE)

    dfout <- data.frame(matrix(nrow = 0, ncol = length(names(x))))
    colnames(dfout) <- names(x)

    # TODO: generalize split for n site or horizon level attributes

    res <- lapply(aqp::split(object, f = group.by.col), function(obj) {

      # TODO: what else would be needed to do similar splits on the horizon data?
      #       essentially, base::split of horizons(obj) + summary on groups within SPC? pretty simple in principle

      #       summarize could operate on groups at multiple levels -- just need another loop in here for horizons
      #       this depends on aqp::split() being able to make splits based on criteria at multiple levels, then
      #       parsing up of the profiles in resulting list elements based on unique levels of horizon-level groups
      #
      #       new behavior of aqp::split would possibly result in a list with overlap of profiles across elements
      #       also, could use diagnostics and or restrictions -- future compositeSPC will include these

      # apply expressions to each group, frameify results
       data <- compositeSPC(obj)

       for(n in names(x)) {
          output <- rlang::eval_tidy(x[[n]], data)
          names(output) <- n

          if(length(output) > 1) {
            stop("summary value '%s' has length greater than one", call.=FALSE)
          } else if(length(output) == 0) {
            stop("summary value '%s' has length zero", call.=FALSE)
          }
          if(any(c(nrow(dfout), length(dfout)) == 0)) {
            dfout <- output
          } else {
            dfout <- cbind(dfout, output)
          }
       }
       return(dfout)
    })

    # recombine results for each group into a single data.frame
    summaries <- do.call('rbind', res)
    final <- data.frame(groups, summaries)
    colnames(final) <- c(group.by.col, names(x))

    # return result in same class as SPC slots
    return(.as.data.frame.aqp(final, aqp_df_class(object)))
  } else {
    stop("package 'rlang' is required for summarize", .call=FALSE)
  }
})

