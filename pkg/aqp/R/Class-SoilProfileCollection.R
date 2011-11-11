setClass(
  Class='SoilProfileCollection', 
  representation=representation(
    idcol='character', # column name containing IDs
    depthcols='character', # 2 element vector with column names for hz top, bottom
    metadata='data.frame', # single-row dataframe with key-value mapping
    horizons='data.frame', # all horizons sorted by ID, top
    site='data.frame', # data about the sampling sites
    sp='SpatialPoints' # (optional) spatial data stored here
  ),
  prototype=prototype(
    idcol='id',
    depthcols=c('top','bottom'),
    metadata=data.frame(stringsAsFactors=FALSE), # default units are unkown
    horizons=data.frame(),
    site=data.frame(),
    sp=new('SpatialPoints')
  ),
  validity=function(object) {

#     # number of ids and number of profiles must match
#     if (length(which(!is.na(object@ids))) != length(object@profiles))
#       stop('number of ids and number of profiles must match')
#     # if there is some site data
#     if (length(object@site) > 0) {
#       # number of ids and number of sites must match
#       if (length(which(!is.na(object@ids))) != nrow(object@site))
# 		 stop('number of ids and number of sites must match')
# 	}
#
    return(TRUE)
  }
)




















