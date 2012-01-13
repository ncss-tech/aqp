
# split an SPC into a list of SPCs, each with 1 profile
if (!isGeneric("splitProfiles"))
 	setGeneric("splitProfiles", function(object) standardGeneric("splitProfiles"))

setMethod(f='splitProfiles', signature='SoilProfileCollection',
	function(object) {
						
		# get profile IDs
		pIDs <- profile_id(object)
					
		# init empty list
		l <- list()
						
		# iterate over SPC, spliting into a list of SPC_i ... SPC_n
		for(i in seq_along(pIDs)) {
			pID <- pIDs[i]
			l[[pID]] <- object[i, ]
		}
						
		return(l)
	}
)



# analog to apply():
# operates by profile, returns vector of length(object) or nrow(object)

if (!isGeneric("profileApply"))
	setGeneric("profileApply", function(object, FUN, ...) standardGeneric("profileApply"))

setMethod(f='profileApply', signature='SoilProfileCollection',
	function(object, FUN, ...) {
		
		# split into a list of 1-profile SPC objects
		s <- splitProfiles(object)
		
		# apply function by SPC_i
		l <- llply(s, .fun=FUN, ...)
		
		# return as vector
		return(unlist(l))
	}
)




