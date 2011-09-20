setClass(
  Class='SoilProfileCollection', 
  representation=representation(
    idcol="character", # column name containing IDs
    topcol="character", # column name containing horizon top boundaries
    bottomcol="character", # column name containing horizon bottom boundaries
    metadata="data.frame", # single-row dataframe with key-value mapping
    horizons="data.frame", # all horizons sorted by ID, top
    site="data.frame", # data about the sampling sites
    sp="SpatialPoints" # (optional) spatial data stored here
  ),
  prototype=prototype(
    idcol='id',
    idcol='top',
    idcol='bottom',
    metadata=data.frame(),
    horizons=data.frame(),
    site=data.frame(),
    sp=SpatialPoints(matrix(c(1,1), nrow=1))
  ),
  validity=function(object) {
    
#     # number of ids and number of profiles must match
#     if (length(which(!is.na(object@ids))) != length(object@profiles))
#       stop("number of ids and number of profiles must match")
#     # if there is some site data
#     if (length(object@site) > 0) {
#       # number of ids and number of sites must match  
#       if (length(which(!is.na(object@ids))) != nrow(object@site))
# 		 stop("number of ids and number of sites must match")
# 	}
#     
    return(TRUE)
  }
)




















