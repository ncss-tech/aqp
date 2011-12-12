test_hz_logic <- function(i, topcol, bottomcol)
  {
  # test for na
  if(any(c(is.na(i[[topcol]])), is.na(i[[bottomcol]]))) {
    res <- FALSE
    names(res) <- 'hz_logic_pass'
    return(res)
  }
  
  # PASSES for now
  else {
    res <- TRUE
    names(res) <- 'hz_logic_pass'
    return(res)
  }
    
  
  ## this will not work when an SPC of discreet slices is generated!
  ## todo: break out into smaller simpler functions, and use only when needed
#   # test hz logic
#   n <- nrow(i)
#   res <- all.equal(i[[topcol]][-1], i[[bottomcol]][-n])
#   if(res != TRUE) {
#     res <- FALSE
#     names(res) <- 'hz_logic_pass'
#   }
  
  # return(res)
  }


.SoilProfileCollectionValidity <- function(object) {
  
    # 1. test for bad horizon logic
    id <- idname(object)
    h <- horizons(object)
    dc <- horizonDepths(object)
    top <- dc[1]
    bottom <- dc[2]
    
    # perform test
    obj.test <- ddply(h, id, test_hz_logic, topcol=top, bottomcol=bottom)
    
    # let the user know which profile IDs aren't going to work
    if(any(obj.test$hz_logic_pass == FALSE)) {
      bad.ids <- obj.test[[id]][obj.test$hz_logic_pass == FALSE]
      msg <- paste('\n\nNOTICE: invalid horizon logic in:', paste(bad.ids, collapse=','), '\n')
      return(msg)
    }
      
    # if all was well
    return(TRUE)
  }

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
    horizons=data.frame(stringsAsFactors=FALSE),
    site=data.frame(stringsAsFactors=FALSE),
    sp=new('SpatialPoints')
  ),
  validity=.SoilProfileCollectionValidity
)




