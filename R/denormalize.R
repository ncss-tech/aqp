# create a (redundant) horizon-level attribute from a site-level attribute
# @ authors: andrew brown & dylan beaudette

denormalize <- function(obj, attr) {
  
  # extract relevant pieces
  h <- horizons(obj)
  s <- site(obj)
  idn <- idname(obj)
  
  # varibles required for join
  vars <- c(idn, attr)
  
  # perform left-join
  # retain only IDs on left side, IDs + attr on the right
  res <- merge(h[, idn, drop=FALSE], s[, vars], all.x = TRUE)
  
  # susbet named attr
  res <- res[[attr]]
  
  return(res)
}
