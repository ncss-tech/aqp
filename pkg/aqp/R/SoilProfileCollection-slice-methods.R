## slice(SPC, ...)

# works on a single set of depths + property at a time
# include:
# 'bottom' - bottom boundary is included in the z-slice test
# 'top' - top boundary is included in the z-slice test
get.slice <- function(d, top, bottom, z, include='top', strict=TRUE) {
  # extract pieces
  d.top <- d[[top]]
  d.bottom <- d[[bottom]]
  d.v <- d[['value']]
  d.var.name <- unique(d[['variable']]) # this is repeated for each horizon

  # determine the property at z-slice, based on boundary rule
  if(include == 'bottom')
    res <- d.v[which(z > d.top & z <= d.bottom)]
  if(include == 'top')
    res <- d.v[which(z >= d.top & z < d.bottom)]
  else
    stop('invalid horizon boundary rule')
  
  # used for QA/QC
  l.res <- length(res)
  
  # account for no data
  if(l.res == 0)
    res <- NA
  
  # if there were multiple matches (i.e. bad horizonation)
  if(l.res > 1 ) {
    
    # strict usage of the data... erors stop execution
    if(strict == TRUE) {
      print(d)
      stop('bad horizonation')  
    }
    
    # looser interp of the data... issue warning and pic the first
    else {
     warning('Bad horizonation detected, using the mean of all matching results. Use strict=TRUE to enforce QA/QC.')
     res <- mean(res, na.rm=TRUE)
    }
    
  }
  
  # name the variable, for nicer column names output from ddply()
  names(res) <- 'slice'
  return(res)
  }


## slice:
if (!isGeneric("slice"))
  setGeneric("slice", function(object, ...) standardGeneric("slice"))


## TODO: this is slower than soil.slot ... why?
## TODO: allow the use of site data (PSC etc.) to determine the z-slice
setMethod(f='slice', signature='SoilProfileCollection',
  function(object, fm, top.down=TRUE, just.the.data=FALSE, progress='none', strict=TRUE){
  
  ## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)
  
  # test for logical input
  if(! inherits(fm, "formula"))
    stop('must provide a valid formula: ~ var1 + var2 + ...')

  # extract components of the formula:
  formula <- str_c(deparse(fm, 500), collapse="")
  elements <- str_split(formula, fixed("~"))[[1]]
  formula <- lapply(str_split(elements, "[+*]"), str_trim)

  # TODO: this will have to be changed when we implement no LHS = all slices
  if (length(formula) > 2)
    stop("please provide a valid formula")

  # extract parsed formula components
  vars <- formula[[2]] # RHS, simple enough
  # LHS: could be either single integer or vector of slices
  z <- as.numeric(eval(parse(text=formula[[1]])))

  # get horizons + depth column names + ID column name
  h <- horizons(object)
  hd <- horizonDepths(object)
  top <- hd[1] ; bottom <- hd[2] # convenience vars
  id <- idname(object)
  
  # get variable classes
  vars.is.numeric.test <- sapply(vars, function(i) is.numeric(h[[i]]))  
  
	# check for bogus left/right side problems with the formula
  if(any(z < 0) | any(is.na(z)))
    stop('z-slice must be >= 1')

  ## TODO: this will have to be updated for z-slices defined by data in @site
	if(! class(z) %in% c('numeric','integer')) # bogus z-slice
		stop('z-slice must be either numeric or integer')

	if(any(vars %in% names(h)) == FALSE) # bogus column names in right-hand side
		stop('column names in formula do not match any horizon data')

  # notify user that a mixture of numeric / categorical vars is not supported
  if(length(unique(vars.is.numeric.test)) > 1) {
     print(vars.is.numeric.test)
     stop('a mixture of numeric/categoric variables is not currently supported')
  }
  
  ## TODO this approach won't work with mixed (numeric / char / factor variables)
  # numeric / categorical vars must be done in different passes
  # melt into long format
  m <- melt(h, measure.vars=vars, id.vars=c(id, top, bottom))
  
  ## melt.data.frame will convert id column into a factor... undo that behavior!
  m[[id]] <- as.character(m[[id]])
  
  # extract slice by id/variable, for each requested depth
  # pre-allocate storage as list
  hd.slices <- vector(mode='list', length=length(z))
  # prepare an index for the list
  slice.idx <- seq_along(z)

  # iterate over this index
  for(slice.i in slice.idx) {
    
    # errors from get.slice() can be avoided loosening constraints with strict=FALSE
    m.i <- ddply(m, c(id, 'variable'), .fun=get.slice, .progress=progress, top=top, bottom=bottom, z=z[slice.i], strict=strict)
    
    # add depth range:
    # top-down, means that the slice starts from the user-defined depths (default)
    if(top.down) {
      m.i[[top]] <- z[slice.i]      # "top"
      m.i[[bottom]] <- z[slice.i] + 1  # "bottom"
    }
    # otherwise, the slice starts at the bottom (why would someone do this?)
    else {
      m.i[[top]] <- z[slice.i] - 1 # "top"
      m.i[[bottom]] <- z[slice.i]     # "bottom"
    }
    # save to the list
    hd.slices[[slice.i]] <- m.i
    }
  
  # convert list into DF and order by id, top = hd[1]
  hd.slices <- ldply(hd.slices)
  
  ## TODO: make sure sorting is correct!  
  hd.slices <- hd.slices[order(hd.slices[[id]], hd.slices[[top]]), ]

  # convert back into wide format
  # and remove reshape-related attributes
  fm.to.wide <- as.formula(paste(id, '+', top, '+', bottom, '~', 'variable', sep=' '))
  hd.slices <- data.frame(cast(hd.slices, formula=fm.to.wide, value='slice'), stringsAsFactors=FALSE)

  # if we just want the data:
  if(just.the.data)
    return(hd.slices)

  # if spatial data and only a single slice: SPDF
  # TODO: include site data as well
  if(nrow(coordinates(object)) == length(object) & length(z) == 1) {
    cat('result is a SpatialPointsDataFrame object\n')
    # check for site data, if present - join to our sliced data
    if(nrow(site(object)) > 0 )
      hd.slices <- join(hd.slices, site(object))
    
    return(SpatialPointsDataFrame(coordinates(object), data=hd.slices))
    }
  else
    cat('result is a SoilProfileCollection object\n')
  
  # otherwise return an SPC, be sure to copy over the spatial data
  depths(hd.slices) <- as.formula(paste(id, '~', top, '+', bottom))
  hd.slices@sp <- object@sp
  
  # if site data: return an SPC + @site
  # note that we should have a proper setter for this
  if(nrow(site(object)) > 0 )
    hd.slices@site <- site(object)
  
  # reset options:
  options(opt.original)
  
  # done
  return(hd.slices)
  }
)
