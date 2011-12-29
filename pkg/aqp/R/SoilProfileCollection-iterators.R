## experimental!!

# analog to apply():
# operates along horizons, by profile
# returning n = nrow(horizons(obj))

# returns a data.frame
if (!isGeneric("profileApply"))
  setGeneric("profileApply", function(object, FUN, ...) standardGeneric("profileApply"))


setMethod(f='profileApply', signature='SoilProfileCollection',
  function(object, FUN, ...){
	
	h <- horizons(object)
	l <- dlply(h, idname(object), .fun=FUN, ...)
	res <- unlist(l)
	
	return(res)
  }
)





