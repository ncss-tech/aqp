
# analog to apply():
# operates by profile, returns vector of length(object)

if (!isGeneric("profileApply"))
	setGeneric("profileApply", function(object, FUN, ...) standardGeneric("profileApply"))

setMethod(f='profileApply', signature='SoilProfileCollection',
	function(object, FUN, ...) {
		# split into a list of 1-profile SPC objects
		s <- split(object)
		# apply function by SPC_i
		l <- llply(s, .fun=FUN, ...)
		# return as vector
		res <- unlist(l)
		return(res)
	}
)




# split an SPC into a list of SPCs, each with 1 profile
setMethod(f='split', signature='SoilProfileCollection',
	function(x) {
		
		# get profile IDs
		pIDs <- profile_id(x)
		
		# init empty list
		l <- list()
		
		# iterate over SPC, spliting into a list of SPC_i ... SPC_n
		for(i in seq_along(pIDs)) {
			pID <- pIDs[i]
			l[[pID]] <- x[i, ]
		}
		
		return(l)
	}
)


