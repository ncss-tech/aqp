# # analog to apply():
# operates by profile, returns vector of length(object) or nrow(object)
if (!isGeneric("profileApply"))
 	setGeneric("profileApply", function(object, FUN, simplify=TRUE, ...) standardGeneric("profileApply"))

setMethod(f = 'profileApply', signature = 'SoilProfileCollection',
	function(object, FUN, simplify = TRUE, ...) {
	  
	  res <- lapply(profiles(object), FUN = FUN, ...)
						
		# optionally simplify
		if(simplify) {
			res <- unlist(res)
		}
		
		return(res)
  }
)
