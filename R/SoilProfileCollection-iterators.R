# analog to apply():
# operates by profile, returns vector of length(object) or nrow(object)
if (!isGeneric("profileApply"))
 	setGeneric("profileApply", function(object, FUN, ...) standardGeneric("profileApply"))

setMethod(f='profileApply', signature='SoilProfileCollection',
	function(object, FUN, ...) {
						
		# get profile IDs
		pIDs <- profile_id(object)
					
		# init empty list
		l <- list()
						
		# iterate over SPC, spliting into a list of SPC_i ... SPC_n
		for(i in seq_along(pIDs)) {
			pID <- pIDs[i]
			l[[pID]] <- do.call(FUN, list(object[i, ]))
		}
		
		# convert list into a vector
		return(unlist(l))
	}
)






