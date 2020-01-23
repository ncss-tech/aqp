# https://github.com/ncss-tech/aqp/issues/75

## init-time validity checks
# too-strict checking precludes analysis of E/B type horizons and common errors
.SoilProfileCollectionValidity <- function(object) {
  
  # over-checking incurs performance penalty
  # for now we do nothing
	return(TRUE)
      
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
    hzdesgncol='character', # column name containing horizon designation
    hztexclcol='character',
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
    hzdesgncol=character(0),
    hztexclcol=character(0),
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

