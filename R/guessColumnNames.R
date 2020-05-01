# guessColumnNames.R
# TODO: document or make internal?

guessHzDesgnName <- function(x) {
  nm <- horizonNames(x)
  name <- NA 
  
  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }
  
  if(length(hzdesgnname(x))) {
    # ideally use @hzdesgncol if it contains a value. if so, no message
    name <- hzdesgnname(x)
  } else {
    # possible names include column names with name in the name
    possible.name <- nm[grep('name', nm, ignore.case=TRUE)]
    
    # use the first valid guess
    if(length(possible.name) > 0) {
      possible.name <- possible.name[1]
      name <- possible.name
      message(paste('guessing horizon designations are stored in `', name, '`', sep=''))
    } else {
      message('unable to guess column containing horizon designations')
    }
  }
  
  return(name)
}

guessHzTexClName <- function(x) {
  nm <- horizonNames(x)
  name <- NA 
  
  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }
  
  if(length(hztexclname(x))) {
    # ideally use @hzdesgncol if it contains a value. if so, no message
    name <- hztexclname(x)
  } else {
    # possible names include column names with name in the name
    possible.name <- nm[grep('texcl', nm, ignore.case=TRUE)]
    
    # use the first valid guess matching texcl
    if(length(possible.name) > 0) {
      possible.name <- possible.name[1]
      name <- possible.name
      message(paste('guessing horizon texture classes are stored in `', name, '`', sep=''))
    } else { 
      # alternately, try for something called "texture"
      possible.name <- nm[grep('texture', nm, ignore.case=TRUE)]
      if(length(possible.name) > 0) {
        possible.name <- possible.name[1]
        name <- possible.name
        message(paste('guessing horizon texture classes are stored in `', name, '`', sep=''))
      } else {
        message('unable to guess column containing horizon texture classes')
      }
    }
  }
  
  return(name)
}

# this works for arbitrary columns where possible/preferred formative elements are known 
#  there is a preference for records where more optional requirements are met to handle cases where there will be many matches
#  for example, working with component data one might have low rv and high total clay, as well as clay fractions

# e.g. guessHzAttrName(x, attr="clay", optional=c("total", "_r")) 
#        matches (claytotal_r == totalclay_r) over (clay_r == claytotal == totalclay) over clay

guessHzAttrName <- function(x, attr, optional) {
  nm <- horizonNames(x)
  
  if(!inherits(x, 'SoilProfileCollection')) {
    stop("x must be a SoilProfileCollection")
  }
  
  # possible names include column names with name in the name
  req <- grepl(attr, nm, ignore.case=TRUE)
  opt <- lapply(as.list(optional), function(i) grepl(i, nm, ignore.case=TRUE))
  opt <- rowSums(do.call('cbind', opt))
  
  if(!any(req)) {
    message(sprintf('unable to guess column containing horizon attribute \'%s\'', attr))
  }
  
  # all optional requirements met
  idx1 <- which(req & opt == length(optional))
  
  # one or more optional requirements met
  idx2 <- which(req & opt > 0)
  
  # basic requirement met
  idx3 <- which(req)
  
  # return first index matching in decreasing precedence
  #  all optional met, some optional met, basic requirement met, no requirement met
  if(length(idx1)) {
    return(nm[idx1[1]])
  } else if(length(idx2)) {
    return(nm[idx2[1]])
  } else if(length(idx3)) {
    return(nm[idx3[1]])
  } else {
    return(NA)
  }
}