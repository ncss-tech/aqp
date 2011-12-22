## slice(SPC, ...)

# this function is run on the horizon data, once for each depth slice
get.slice.parallel <- function(h, id, top, bottom, vars, z, include='top', strict=TRUE) {
  
  # 1. get indices to rows matchings current depth slice (z)
  # this is the default method
  if(include == 'top')
    idx <- which(z >= h[[top]] & z < h[[bottom]])  
  # not sure why someone would use this approach, but include it anyways  
  if(include == 'bottom')
  	idx <- which(z > h[[top]] & z <= h[[bottom]])

  # 2. extract data.frame along slice, and named vars + id
  h <- h[idx, c(id, vars)]
  
  # 3. QA/QC
  # how many unique IDs?
  l.ids <- length(unique(h[[id]]))   
  # how many rows in the result?
  n.res <- nrow(h)
  
  # more rows than IDs --> bad horizonation
  if(l.ids != n.res) {
  	if(strict == TRUE) {
  	  # get offending ID
  	  id.tab <- table(h[[id]])
  	  print(id.tab)
  	  bad.ids <- paste(names(id.tab)[which(id.tab > 1)], collapse=',')
  	  stop(paste('bad horizonation in IDs:', bad.ids))
  	  }
  	
  	# looser interp of the data... issue warning and return multuple rows/ID
  	# join(..., match='first') will correct the problem
    else
      warning('Bad horizonation detected, first matching horizon selected. Use strict=TRUE to enforce QA/QC.')
  	}
  
  # done: return subset of original data
  return(h)
  }


## this is a much more robust + fast version of slice.slow
## needs a little more testing, and then will be ready
slice.fast <- function(object, fm, top.down=TRUE, just.the.data=FALSE, progress='none', strict=TRUE){
  
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
  id.order <- profile_id(object) # this is the original ordering of profiles
    
  # check for bogus left/right side problems with the formula
  if(any(z < 0) | any(is.na(z)))
    stop('z-slice must be >= 1')

  ## TODO: this will have to be updated for z-slices defined by data in @site
  if(! class(z) %in% c('numeric','integer')) # bogus z-slice
		stop('z-slice must be either numeric or integer')

  if(any(vars %in% names(h)) == FALSE) # bogus column names in right-hand side
		stop('column names in formula do not match any horizon data')

  
  ## extract all vars by slice_i
  # pre-allocate storage as list
  hd.slices <- vector(mode='list', length=length(z))
  # prepare an index for the list
  slice.idx <- seq_along(z)
  
  # iterate over this index
  for(slice.i in slice.idx) {
    
    # extract all vars for current slice
    m.i.sub <- get.slice.parallel(h, id=id, top=top, bottom=bottom, vars=c(id, vars), z=z[slice.i], strict=strict)
    
    # join with original IDs in order to account for NA, or bad horizonation
    d <- data.frame(temp_id=id.order)
    names(d) <- id
    m.i <- join(d, m.i.sub, type='left', match='first')
    
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
  
  # convert list into DF
  hd.slices <- ldply(hd.slices)
  
  # re-order by id, then top
  # keep only data we care about
  hd.slices <- hd.slices[order(match(hd.slices[[id]], id.order), hd.slices[[top]]), c(id, top, bottom, vars)]
  
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


slice.slow <- function(object, fm, top.down=TRUE, just.the.data=FALSE, progress='none', strict=TRUE){
  
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
  id.order <- profile_id(object) # this is the original ordering of profiles
  
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
    
    ## wow this is stupid... ddply() is re-ordering the data
    ## this is the most time consuming chunk of code
    ## a vectorized version of get.slice would help
    # errors from get.slice() can be avoided by loosening constraints with strict=FALSE
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
  
  
  ## TODO: this is wasteful
  # convert list into DF and order by id, top = hd[1]
  hd.slices <- ldply(hd.slices)
  
  ## stupid: this is re-ordering my data as well
  # convert back into wide format
  # and remove reshape-related attributes
  fm.to.wide <- as.formula(paste(id, '+', top, '+', bottom, '~', 'variable', sep=' '))
  hd.slices <- data.frame(cast(hd.slices, formula=fm.to.wide, value='slice'), stringsAsFactors=FALSE)
  
  ## fix the broken ordering from ddply(), and cast()  
  hd.slices <- hd.slices[order(match(hd.slices[[id]], id.order), hd.slices[[top]]), ]
  
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


## slice:
if (!isGeneric("slice"))
  setGeneric("slice", function(object, ...) standardGeneric("slice"))


## TODO: this is slower than soil.slot ... why?
## TODO: allow the use of site data (PSC etc.) to determine the z-slice
setMethod(f='slice', signature='SoilProfileCollection', slice.slow)
