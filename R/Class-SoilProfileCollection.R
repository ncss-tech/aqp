
setClass(
  Class = 'SoilProfileCollection',
  representation = representation(
    idcol = 'character', # column name containing IDs
    hzidcol = 'character', # column name containing unique horizon IDs
    
    hzdesgncol = 'character', # column name containing horizon designation
    hztexclcol = 'character', # column name containing horizon texture class
    
    depthcols = 'character', # 2 element vector with column names for hz top, bottom
    
    metadata = 'list', # list with key-value mapping
    
    horizons = 'data.frame', # all horizons sorted by ID & top depth
    
    site = 'data.frame', # data about the sampling sites
    
    sp = 'SpatialPoints', # spatial data stored here, initialized as 'empty' sp object
    
    diagnostic = 'data.frame',# (optional) diagnostic horizons are stored here
    restrictions = 'data.frame' # (optional) restrictions are stored here
  ),
  prototype = prototype(
    idcol = 'id',
    hzidcol = 'hzID',
    hzdesgncol = character(0),
    hztexclcol = character(0),
    depthcols = c('top', 'bottom'),
    metadata = list(aqp_df_class = "data.frame", 
                          stringsAsFactors = FALSE),
    horizons = data.frame(id  = character(0), hzID = character(0),
                          top = numeric(0), bottom = numeric(0),
                          stringsAsFactors = FALSE),
    site = data.frame(id = character(0), stringsAsFactors = FALSE),
    sp = SpatialPoints(data.frame(x = 0, y = 0))[-1, ],
    diagnostic = data.frame(stringsAsFactors = FALSE),
    restrictions = data.frame(stringsAsFactors = FALSE)
  ),
  validity = function(object) {
    return(spc_in_sync(object)$valid)
  }
)

# https://github.com/ncss-tech/aqp/issues/75
## init-time validity checks
# too-strict checking precludes analysis of E/B type horizons and common errors and over-checking incurs performance penalty
# .SoilProfileCollectionValidity <- function(object) {
#   # over-checking incurs performance penalty
#   # for now we do nothing
#   return(TRUE)
# }

##
## notes:
##

# 2019-03-15: creating an empty SpatialPoints object requires more effort
# c/o: https://gis.stackexchange.com/questions/291069/creating-empty-spatialpoints-or-spatialpointsdataframe-in-r
# old: new('SpatialPoints')
# new: SpatialPoints(data.frame(x = 0, y = 0))[-1,]


# 2020-05-30: make data.table, tbl_df and data.frame slots "co-exist"
#
# see: https://stackoverflow.com/questions/35642191/tbl-df-with-s4-object-slots
setOldClass(c("data.table", "data.frame"))
setOldClass(c("tbl_df", "tbl", "data.frame"))

# define a safe generic to facilitate coercion back to parent object class
if (!isGeneric('.as.data.frame.aqp'))
  setGeneric('.as.data.frame.aqp', function(x, as.class, ...) {
    standardGeneric('.as.data.frame.aqp')
  })

setMethod(".as.data.frame.aqp", signature(x = "ANY"),
          function(x, as.class = "data.frame", ...) {
            
            # 2020-05-30: sub-classes of data.frame have more than one class
            # debug
#            if (as.class == 'data.frame')
#              stop("foo")

            # don't invoke coercion methods if not needed
            if (!inherits(x, 'data.frame')) {
              warning("input data class does not inherit from `data.frame`", call.=TRUE)
              return(x)
            }
            
            # NULL x -- probably from unusual use cases
            if (is.null(class(x)))
              stop(sprintf("input object is NULL, expected '%s'", as.class))
            
            # note: we handle the possibly NULL/0-length as.class
            #       by letting it fall through default switch EXPR
            #       a warning is generated for non-data.frames
            cond <- class(x)[1] == as.class
            
            if (all(length(cond) > 0 & cond)) {
              
              # rm rownames in slots
              rownames(x) <- NULL
              
              return(x)
            }
            
            switch(as.class,
              'data.table' = {
                #print(as.class)
                if (requireNamespace("data.table"))
                  return(data.table::as.data.table(x, ...))
                message(
                  "using data.table class in SoilProfileCollection slots requires the `data.table` package"
                )
              },
              'tbl_df' = {
                #print(as.class)
                if (requireNamespace("tibble"))
                  return(tibble::as_tibble(x, ...))
                message(
                  "using tbl_df class in SoilProfileCollection slots requires the `tibble` package"
                )
              },
              {
                # default/data.frame
                #  if we were supposed to get something else,
                #  make a warning with a stack trace
                if (as.class != "data.frame") {
                  message(sprintf("failed to use %s as data.frame class", as.class))
                  warning("data.table and tbl_df in SoilProfileCollection data.frame slots are EXPERIMENTAL, defaulting to data.frame", call. = FALSE)
                }
                
                # return data.frame no matter what
                res <- as.data.frame(x, ...)
                
                # rm rownames in slots
                rownames(res) <- NULL
                
                return(res) 
              }
              )
          })

