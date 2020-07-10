# simple wrapper to mpspline2::mpspline
# site data are preserved
# diagnostic horizons are lost
# spatial data are lost
# metadata are lost
#
## TODO: extend to multiple variables in a single call to function

# p: SPC
# var: horizon-level attribute name
# d: depths to re-sample var
# ...: passed to mpspline()
mpsplineSPC <- function(p, var, d, ...) {
  
  # https://github.com/obrl-soil/mpspline2
  p.mps <- mpspline(p, var_name = var, d = d, ...)
  
  # convert output from mpspline to list of SPCs
  z <- lapply(p.mps, .singleMPS2SPC, d=d, id=idname(p), hd=horizonDepths(p), varname=var)
  
  # union into single SPC
  z <- union(z)
  
  # splice-in original site data via left-join
  p.site <- site(p)
  names(p.site)[which(names(p.site) == idname(p))] <- '.original_id'
  site(z) <- p.site 
  
  # combine with original data
  z <- union(list(z, p))
  
  # remove temporary site id
  z$.original_id <- NULL
  
  # init new site level attr with id-grouping
  site(z)$id_group <- NA
  # iterate over ids, and assign groups
  for(i in profile_id(p)) {
    idx <- grep(i, profile_id(z))
    z$id_group[idx] <- i
  }
  
  # init new site level attr with method-grouping
  site(z)$method_group <- 'original'
  z$method_group[grep('-1cm', profile_id(z), fixed = TRUE)] <- '1cm'
  z$method_group[grep('-res', profile_id(z), fixed = TRUE)] <- 'res'
  
  # set levels likely suitable for most plotting
  z$method_group <- factor(z$method_group, levels = c('original', '1cm', 'res'))
  
  return(z)
}


# expecting mpspline's new list output
# s: single profile
# d: resampled depths
# id: id name from source SPC
# hd: horizon top/bottom field names
# varname: horizon level attr name
.singleMPS2SPC <- function(s, d, id, hd, varname) {
  
  # first SPC to make: 1cm intervals
  
  # truncate to depth of real data
  # TODO: this will break if MPS contain NA (?)
  not.NA.idx <- which(!is.na(s$est_1cm))
  
  # init new DF with depths
  s.1cm <- data.frame(top=0:(length(s$est_1cm[not.NA.idx]) - 1), bottom=1:(length(s$est_1cm[not.NA.idx])))
  
  # basic structure must match source SPC
  # id is modified for uniqueness
  names(s.1cm) <- hd
  s.1cm[[id]] <- sprintf("%s-1cm", s$id)
  s.1cm[[varname]] <- s$est_1cm[not.NA.idx]
  
  # upgrade to SPC
  fm <- as.formula(sprintf("%s ~ %s + %s", id, hd[1], hd[2]))
  depths(s.1cm) <- fm
  
  # add original id for linking site data
  site(s.1cm)$.original_id <- s$id
  
  # second SPC to make: resmpled depths
  # use all depths for now, even those with all NA
  s.r <- data.frame(top=d[-length(d)], bottom=d[-1])
  
  # basic structure must match source SPC
  # id is modified for uniqueness
  names(s.r) <- hd
  s.r[[id]] <- sprintf("%s-res", s$id)
  s.r[[varname]] <- s$est_dcm
  
  # upgrade
  depths(s.r) <- fm
  
  # add original id for linking site data
  site(s.r)$.original_id <- s$id
  
  # combine
  res <- union(list(s.1cm, s.r))
  
  return(res)
}
