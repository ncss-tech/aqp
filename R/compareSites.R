

#' @title Compare Site Level Attributes of a SoilProfileCollection
#' 
#' @description Compare site level attributes of a `SoilProfileCollection` object, returning a distance matrix conformal with the output from [NCSP()].
#' 
#' @param x `SoilProfileCollection` object
#' @param vars character vector listing one or more site level attributes of `x`
#' @param weights numeric vector, same length as `vars`, variable weighting
#' @param ... additional arguments to [cluster::daisy()]
#'
#' @return `dissimilarity` / `dist` class object containing pair-wise distances, row/column names derived from `profile_id(x)`
#' 
#' @export
#' 
#' @seealso [NCSP()] [cluster::daisy()]
#' 
compareSites <- function(x, vars, weights = rep(1, times = length(vars)), ...) {
  
  # extract 1 or more site variabels
  .s <- site(x)[, vars, drop = FALSE]
  
  # transfer profile IDs -> row names
  row.names(.s) <- profile_id(x)
  
  # wrap cluster::daisy
  .d <- cluster::daisy(.s, metric = 'gower', weights = weights, ...)
  
  .d <- as.dist(.d)
  
  # standardize attributes to match NCSP()
  attr(.d, 'Metric') <- 'Gower'
  
  return(.d)  
}
