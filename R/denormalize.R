# create a (redundant) horizon-level attribute from a site-level attribute
# @ authors: andrew brown & dylan beaudette

denormalize <- function(obj, attr) {
  
  # extract relevant pieces
  h <- horizons(obj)
  s <- site(obj)
  
  if(!attr %in% names(s))
    stop("column name %s not found", call. = FALSE)
  
  # make a lookup table of attr in site
  lut <- s[[attr]]
  names(lut) <- s[[idname(obj)]]
  
  # return a horizon level attribute for same site IDs
  return(lut[h[[idname(obj)]]])
}
