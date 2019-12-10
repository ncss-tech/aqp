# functions for argillic horizon detection in SPCs
# getArgillicBounds - upper and lower boundary of argillic horizon
# crit.clay.argillic - argillic horizon threshold function
# argillic.clay.increase.depth() - top depth of argillic

getArgillicBounds <- function(p, hzdesgn='hzname', attr = 'clay',
                              require_t = TRUE, 
                              bottom.pattern = "Cr|R|Cd",
                              lower.grad.pattern = "^[2-9]*B*CB*[^rtd]*[1-9]*$",
                              as.list = FALSE) {
  # get upper bound...
  upper.bound <- argillic.clay.increase.depth(p, attr)
  lower.bound <- -Inf
  hz <- horizons(p)
  # if upper.bound is non-NA, we might have argillic, because clay increase is met at some depth
  if(!is.na(upper.bound)) {
    # find all horizons with t subscripts; some old/converted horizons have all capital letters
    has_t <- grepl(as.character(hz[[hzdesgn]]), pattern="[Tt]")
    ########
    # TODO: allow evidence of illuviation from lab data fine clay ratios etc? how?
    #
    # TODO: if `require_t` is set to `FALSE`... or in a future getKandicBounds()...
    #       how do you detect the bottom of a argillic or kandic horizon (which need not have clay films) in e.g. saprolite ?
    #       could you look for C in master horizon designation for e.g. rock structure / parent material?
    #       in lieu of checking for t and a cemented bedrock contact... is that how lower.bound should be determined?
    ########
    if(sum(has_t) | !require_t) {
      # get index of last horizon with t 
      idx.last <- rev(which(has_t))[1] 
      
      ## Partial fix for TODO #2? seems reasnable for the require_t=FALSE lower.bound case
      # take _very_ last horizon depth first (will be truncated to contact depth if needed)
      depth.last <- hz[nrow(hz), horizonDepths(p)[2]]
      
      # take the top depth of any B or C horizon  without t subscript above "depth.last"
      c.idx <- which(grepl(hz[[hzdesgn]], pattern=lower.grad.pattern))
      if(length(c.idx)) {
        c.horizon <- hz[c.idx[1], horizonDepths(p)[1]]
        # if the _shallowest C horizon_ top depth is above the _last horizon_ bottom depth (could be top depth of same hz)
        if(c.horizon < depth.last)  {
          # use the top depth of the first C horizon that matched the pattern
          print(paste0("Lower gradational horizons (peiid: ", site(p)$peiid, ") may be present below argillic. Adjusting lower bound."))
          # plot(p)
          # print(c.idx)
          depth.last <- c.horizon
        }       
      }
      
      # get the bottom depth of the last horizon with a t (this could be same as c.horizon above)
      if(require_t)
        depth.last <- hz[idx.last, horizonDepths(p)[2]]
      
      # in rare cases, the bottom depth of the bedrock/contact is not populated
      # step back until we find one that is not NA
      idx.last.i <- idx.last
      while(is.na(depth.last)) {
        depth.last <- hz[idx.last.i, horizonDepths(p)[2]]
        idx.last.i <- idx.last.i - 1
      }
      
      # estimate the thickness of the soil profile 
      # (you will need to specify alternate pattern if Cr|R|Cd 
      # doesn't match your contacts)
      soil.depth <- estimateSoilDepth(p, name = hzdesgn, top = horizonDepths(p)[1], 
                                      bottom = horizonDepths(p)[2], 
                                      p = bottom.pattern)
      
      # if the last horizon with a t is below the contact (Crt or Rt) or some other weird reason
      if(soil.depth < depth.last) {
        # return the soil depth to contact
        lower.bound <- soil.depth
        
      } else {
        #otherwise, return the bottom depth of the last horizon with a t
        lower.bound <- depth.last
      }
    } else {
      print(paste0("Pedon (",profile_id(p),") meets clay increase but lacks evidence of illuviation (t subscript). Set `require_t=FALSE` to ignore."))
      lower.bound <- NA 
      upper.bound <- NA 
    }
  } else {
    
    # if the upper bound is NA, return NA for the lower bound
    lower.bound <- NA 
  }
  
  if(!is.finite(lower.bound))
    lower.bound <- NA
  
  if(!as.list)
    return(c(upper.bound, lower.bound))
  
  res <- list(ubound=as.numeric(upper.bound), lbound=as.numeric(lower.bound))
  return(res)
}

crit.clay.argillic <- function(eluvial_clay_content) {
  # eluvial clay content is a numeric vector or matrix subsettable with logical vectors based on clay (NA omitted)
  buf <- eluvial_clay_content
  idx.mask <- is.na(buf)
  buf[idx.mask] <- 0
  
  idx.lt15 <- buf < 15
  idx.lt15[idx.mask] <- FALSE
  
  idx.gt40 <- buf >= 40
  idx.gt40[idx.mask] <- FALSE
  
  idx.other <- (buf >= 15) & (buf < 40)
  idx.other[idx.mask] <- FALSE
  
  buf[idx.lt15] <- eluvial_clay_content[idx.lt15] + 3
  buf[idx.gt40] <- eluvial_clay_content[idx.gt40] + 8
  buf[idx.other] <- 1.2*eluvial_clay_content[idx.other]
  return(buf)
}

# returns the top and bottom depth of the argillic horizon as a numeric vector.
# optional argument as.list will return top and bottom depth as a list
# applies get.increase.depth() identify the argillic horizon upper bound
# threshold fun()=`crit.clay.argillic` defines the clay increase that must be met within 30 cm vertical distance
# the default horizon attribute name is `clay`, but it can be adjusted as needed
argillic.clay.increase.depth <- function(p, attr='clay') {
  vd <- 30
  return(get.increase.depths(p, attr = attr, 
                          threshold.fun = crit.clay.argillic, 
                          vertical.distance = vd)[1])
}

