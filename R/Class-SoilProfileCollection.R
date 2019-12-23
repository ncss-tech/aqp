# https://github.com/ncss-tech/aqp/issues/75
# class def for main class within aqp
.SoilProfileCollectionValidity <- function(object) {
	
# 	# for now, test only for NA in horizon boundaries
# 	dc <- horizonDepths(object)
# 	h <- horizons(object)
# 	top <- dc[1]
#   bottom <- dc[2]
# 	
# 	if(any(c(is.na(h[[top]]), is.na(h[[bottom]])))) {
# 		msg <- 'horizon top and bottom values cannot contain NA'
# 		return(FALSE)
# 	}
	
	# otherwise, we are fine
	return(TRUE)
	
	
	## 2013-03-07: this should all be done outside of our class... too much testing slows things down
	
#     # 1. test for bad horizon logic
#     id <- idname(object)
#     h <- horizons(object)
#     dc <- horizonDepths(object)
#     top <- dc[1]
#     bottom <- dc[2]
#     
#     ## Note: this may be redundant, as checking is commonly done before init of object
#     # perform test: fails on missing horizon boundaries or overlapping horizons
#     # non-contiguous horizonation is allowed
#     obj.test <- ddply(h, id, test_hz_logic, topcol=top, bottomcol=bottom)
#     
#     # let the user know which profile IDs aren't going to work
#     if(any(obj.test$hz_logic_pass == FALSE)) {
#       bad.ids <- obj.test[[id]][obj.test$hz_logic_pass == FALSE]
#       msg <- paste('\n\nNOTICE: invalid horizon logic in:', paste(bad.ids, collapse=','), '\n')
#       return(msg)
#     }
      
  }

##
## notes:
##

# 2019-03-15: creating an empty SpatialPoints object requires more effort
# c/o: https://gis.stackexchange.com/questions/291069/creating-empty-spatialpoints-or-spatialpointsdataframe-in-r
# old: new('SpatialPoints')
# new: SpatialPoints(data.frame(x = 0, y = 0))[-1,]

setClass(
  Class='SoilProfileCollection', 
  representation=representation(
    idcol='character', # column name containing IDs
    hzidcol='character', # column name containing unique horizon IDs
    depthcols='character', # 2 element vector with column names for hz top, bottom
    metadata='data.frame', # single-row dataframe with key-value mapping
    horizons='data.frame', # all horizons sorted by ID, top
    site='data.frame', # data about the sampling sites
    sp='SpatialPoints', # spatial data stored here, initialized as 'empty' SP object
    diagnostic='data.frame', # (optional) diagnostic horizons are stored here
    restrictions='data.frame' # (optional) restrictions are stored here
  ),
  prototype=prototype(
    idcol='id',
    hzidcol='hzID',
    depthcols=c('top','bottom'),
    metadata=data.frame(stringsAsFactors=FALSE), # default units are unkown
    horizons=data.frame(stringsAsFactors=FALSE),
    site=data.frame(stringsAsFactors=FALSE),
    sp=SpatialPoints(data.frame(x = 0, y = 0))[-1,],
    diagnostic=data.frame(stringsAsFactors=FALSE),
    restrictions=data.frame(stringsAsFactors=FALSE)
  ),
  validity=.SoilProfileCollectionValidity
)

