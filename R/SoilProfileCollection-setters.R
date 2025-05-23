# setters -- functions that substantially modify data.frame slot contents

##
## depths<- setter method - to create AQP objects: sorts based on ID and top depth
##
# if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) standardGeneric('depths<-'))


#' @aliases depths<-,SoilProfileCollection-method
#' @rdname depths
#' @export
setReplaceMethod("depths", signature(object = "SoilProfileCollection"),
	function(object, value) {
		message('This is already a SoilProfileCollection-class object, doing nothing.')
		object
	})

#' Initialize a SoilProfileCollection from data.frame 
#' @description `depths(<data.frame>) <- <formula>`: Initialize SoilProfileCollection
#' @description `depths(<SoilProfileCollection>)`: Extract profile ID and horizon depths from SoilProfileCollection
#' @name depths<-
#' @param object An object to promote to SoilProfileCollection (inherits from data.frame)
#' @param value A formula specifying the unique profile ID, top and bottom depth column names
#' @aliases depths<-,data.frame-method depths depths<-
#' @details The input horizon data, and the resulting profile order, is sorted based on unique profile ID and top depth. ID columns are converted to character, depth columns are converted to integer. If `NA` values exist in all of the top depths, a prototype with 1 horizon per profile ID is returned, with `NA` in all non-essential columns. If the input `object` has 0 rows, a prototype with 0 horizons and 0 rows, but same column names as `object`, is returned. 
#' @rdname depths
#' @export
#' @examples
#' ## init SoilProfileCollection objects from data.frame of horizon data
#'
#' # load demo data
#' data(sp1)
#'
#' # promote to SPC
#' depths(sp1) <- id ~ top + bottom
#'
#' # plot
#' plot(sp1)
#'
#' # number of profiles
#' length(sp1)
#'
#' # number of horizons
#' nrow(sp1)
#'
setReplaceMethod("depths", "data.frame",
                           # or a data.frame like obj
  function(object, value) {
    if (inherits(value, "formula")) { # initialization by formula:  id ~ top + bottom
      mf <- model.frame(value, object)
      res <- .initSPCfromMF(data = object, mf = mf)
    } else {
      if (inherits(value, "character") &
          inherits(object, 'data.frame')) { # initialization by colnames: c("id","top","bottom")
	      mf <- .data.frame.j(object, value, class(object)[1])
	      res <- .initSPCfromMF(data = object, mf = mf)
      } else {
	      stop('invalid initial object for promotion to SoilProfileCollection', call. = FALSE)
      }
    }

    # add default site data: profile IDs in site same order as horizons
    site.temp <- data.frame(id = profile_id(res), stringsAsFactors = FALSE)
    names(site.temp) <- idname(res)
    adc <- aqp_df_class(res)
    res@site <- .as.data.frame.aqp(site.temp, adc)
    res@horizons <- .as.data.frame.aqp(res@horizons, adc)

    # done
    return(res)
  }
)

.checkNAdepths <- function(depth, l) {
  if(is.factor(depth)) {
    warning(sprintf("Horizon %s depth is a factor! This happens with automatic character to factor conversions!", l), call. = FALSE)

    # this ensures numeric conversion uses factor labels, not levels
    depth <- as.character(depth)
  }
  if(!is.numeric(depth)) {
    depth <- suppressWarnings(try(as.numeric(depth)))
    if(!inherits(depth, 'try-error')) {
      message(sprintf("Horizon %s depths converted to numeric.", l))
    } else stop(sprintf("Unable to convert %s depths to numeric!", l))
  }
  if(any(is.na(depth)))
    warning(sprintf("Horizon %s depths contain NA! Check depth logic with aqp::checkHzDepthLogic()", l), call. = FALSE)
  return(depth)
}

.checkDepthOrder <- function(x, depthcols) {
  if (any(x[[depthcols[2]]] < x[[depthcols[1]]], na.rm = TRUE)) {
    warning("One or more horizon bottom depths are shallower than top depth. Check depth logic with aqp::checkHzDepthLogic()", call. = FALSE)
  }
}

.screenDepths <- function(x, depthcols = horizonDepths(x)) {
  .checkNAdepths(x[[depthcols[1]]], "top")
  .checkNAdepths(x[[depthcols[2]]], "bottom")
  .checkDepthOrder(x, depthcols)
}

# create 0-length spc from id and horizon depth columns (`idn`, `hzd`)
#  - allows template horizon (`hz`) and site (`st`) data to be provided (for additional columns)
.prototypeSPC <- function(idn, hzd, 
                          hz = data.frame(), st = data.frame(), 
                          fill_top = NA_integer_, fill_bottom = NA_integer_) {
  
  # create bare minimum data
  id <- hz[[idn]]
  nid <- length(id)
  iid <- seq_along(id)
  nuhz <- data.frame(id = as.character(id), 
                     hzID = iid, 
                     top = fill_top[nid],
                     bottom = fill_bottom[nid], 
                     stringsAsFactors = FALSE)
  # use idname/horizon depths
  colnames(nuhz) <- c(idn, "hzID", hzd)
  
  # dummy site data
  nust <- data.frame(id = as.character(id), 
                     stringsAsFactors = FALSE)
  colnames(nust) <- idn
  
  # add other columns
  if (ncol(hz) > 0) {
    hz$.dummyVar <- ""[nrow(hz)]
    nuhz <- cbind(nuhz, hz[0, !colnames(hz) %in% colnames(nuhz), drop = FALSE][iid,])
    nuhz$.dummyVar <- NULL
  }
  
  if (ncol(st) > 0) {
    st$.dummyVar <- ""[nrow(st)]
    nust <- cbind(nust, st[0, !colnames(st) %in% colnames(nust), drop = FALSE][iid,])
    nust$.dummyVar <- NULL
  }
    
  # return 0-length or n-length (ID only) SPC
  return(SoilProfileCollection(idcol = idn, 
                               depthcols = hzd,
                               horizons = nuhz,
                               site = nust))
}

##
## initialize SP/SPC objects from a model.frame
## - data is a horizon-level data.frame
## - mf is a model frame expressing the id ~ top + bottom formula
## - use_class is the data.frame class to use ("data.frame", "tbl_df", or "data.table")
## 
# TODO: top and bottom are depth used to fill in an empty horizon for profiles where all top depths are missing. Default `NA_integer_`. Not exposed through depths<- at this time because it only covers the case where every single profile in the collection has all missing top depths. Doing more involved inspection of the SPC inputs to find cases that may have multiple NA-horizons per profile, removing extras, splicing in clean NA-filled data may be more than we want to do in the object constructing method
.initSPCfromMF <- function(data, mf, use_class, top = NA_integer_, bottom = NA_integer_) {
  # get column names containing id, top, bottom
  nm <- names(mf)

  # check for factor-class ID
  if (inherits(data[[nm[1]]], 'factor')) {
    message('converting profile IDs from factor to character')
    data[[nm[1]]] <- as.character(data[[nm[1]]])
  }

  # check for integer IDs
  if (inherits(data[[nm[1]]], 'integer')) {
    message('converting profile IDs from integer to character')
    data[[nm[1]]] <- as.character(data[[nm[1]]])
  }

  # depth column names
  depthcols <- c(nm[2], nm[3])

  # get column containing IDs
  data_id <- data[[nm[1]]]

  # check for all depths NA or 0-length
  if (all(is.na(data[[depthcols[1]]]))) {
    # all top depths missing is intractable, all bottom depths is fixable
    warning("all top depths missing from input data", call. = FALSE)
    # return a filled (n-length) SPC prototype from the horizon data
    return(.prototypeSPC(nm[1], depthcols, hz = data, fill_top = top, fill_bottom = bottom))
  } else if(nrow(data) == 0) {
    warning("input data have 0 rows", call. = FALSE)
    # return a empty (0-length) SPC prototype from the horizon data
    return(.prototypeSPC(nm[1], depthcols, hz = data, fill_top = top, fill_bottom = bottom))
  }
  
  # enforce numeric depths and provide QC warnings as needed
  data[[depthcols[1]]] <- .checkNAdepths(data[[depthcols[1]]], "top")
  data[[depthcols[2]]] <- .checkNAdepths(data[[depthcols[2]]], "bottom")
  
  # warn if bottom depth shallower than top (old style O horizons, data entry issues, etc.)
  .checkDepthOrder(data, depthcols)
  
  tdep <- data[[depthcols[1]]]

  # calculate ID-top depth order, re-order input data
  id_tdep_order <- order(as.character(data_id), tdep)
  data <- data[id_tdep_order,]

  # create a site table with just IDs
  nusite <- .as.data.frame.aqp(data.frame(rle(data[[nm[1]]])$values,
                                          stringsAsFactors = FALSE), class(data)[1])
  names(nusite) <- nm[1]

  # create object
  res <- SoilProfileCollection(idcol = nm[1],
                               hzidcol = 'hzID',
                               depthcols = depthcols,
                               site = nusite,
                               horizons = data)

  
  # check for horizon ID name conflict
  if (hzidname(res) %in% names(data)) {

    # original hz ID
    o.hzid <- hzidname(res)

    # is this a good candidate horizon ID?
    res.status <- try(hzID(res) <- data[[o.hzid]], silent = TRUE)

    # if not, re-make one
    if(inherits(res.status, 'try-error')) {
      # add unique horizon IDs to a new column
      n.hzid <- sprintf("%s_", o.hzid)

      # add non-conflicting hz ID
      res@horizons[[n.hzid]] <- as.character(1:nrow(res))

      # update object
      hzidname(res) <- n.hzid

      # notify
      warning(sprintf('`%s` is not a unique horizon ID, using `%s`', o.hzid, n.hzid), call. = FALSE)
    } else {
      # notify that everything is fine
      message(sprintf('using `%s` as a unique horizon ID', o.hzid))
    }

  } else {
    # no conflict, add a reasonable horizon ID
    hzID(res) <- as.character(1:nrow(res))
  }

  # done
  return(res)
}

setGeneric('site<-', function(object, value)
  standardGeneric('site<-'))

#' Create or Add Data to Site Slot
#'
#' @name site<-
#'
#' @description
#' There are two options available via the \code{site<-} setter.
#'
#' The first is a "normalization" by formula interface, whereby one specifies an attribute that is constant in horizons within profiles to be promoted to a site-level variable: \code{site(spc) <- ~ horizonvariable}
#'
#' The second is creation of site data from an external \code{data.frame} via merge (LEFT JOIN). There must be one or more same-named columns (with at least some matching data) on the left and right hand side to facilitate the join: \code{site(spc) <- newdata}
#'
#' @param object A SoilProfileCollection
#' @param value A formula or object inheriting \code{data.frame}
#' @aliases site<-,SoilProfileCollection-method
#' @usage site(object) <- value
#'
#' @rdname site
#' @export
#' @examples
#'
#' # load test data
#' data(sp2)
#'
#' # promote to SPC
#' depths(sp2) <- id ~ top + bottom
#'
#' # normalize a horizon-level attribute to site
#' site(sp2) <- ~ surface
#'
#' # inspect site table
#' site(sp2)
#'
#' # make some data: classify two geomorphic surfaces with numeric value
#' newdata <- data.frame(surface = c("holocene",
#'                                   "lower riverbank"),
#'                       newvalue = c(1,2))
#'
#' # do left join based on newly-normalized "surface" attribute
#' site(sp2) <- newdata
#'
#' # inspect site table: holocene & lower riverbank have values
#' site(sp2)
#'
setReplaceMethod("site", signature(object = "SoilProfileCollection"),
  function(object, value) {

    # get profile IDs from horizon table
    ids <- as.character(horizons(object)[[idname(object)]])
    pid <- rle(ids)$values
    idn <- idname(object)
    adf <- aqp_df_class(object)
    
  	# creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, object@horizons, na.action = na.pass)
      nm <- names(mf)
      mf <- data.frame(ids, mf, stringsAsFactors = FALSE)
      names(mf) <- c(idn, nm)
      object <- .createSiteFromHorizon(object, mf)
    }

    # creation of site data from an external data.frame via merge (LEFT JOIN)
    if (inherits(value, "data.frame")) {
      # get column names from proposed site, and existing horizons
      new.cols <- names(value)
      hz.names <- horizonNames(object)
      si.names <- siteNames(object)
      
      # short circuit:ensure that site(x)$<-NULL works
      if (all(new.cols %in% si.names) &
               idn %in% new.cols & 
                nrow(value) == length(object)) {
        
          if (!all(value[[idn]] %in% pid)) {
            message("Some profile IDs in input data are not present in object and no new columns to merge. Doing nothing.")
            return(object)
          }
        
          # re-sorts for case when "joining" only ID 
          sort.idx <- match(pid, value[[idn]])
          object@site <- .as.data.frame.aqp(value[sort.idx,, drop = FALSE], adf)
          return(object)
      }
      
      # check there is >1 column shared b/w existing site and value
      if (length(new.cols) > 1 && !any(new.cols %in% si.names)) {
        stop("new data must have one or more column names in common with the site data", call. = FALSE)
      }
      
      ## remove ID column from names(horizons)
      ID.idx <- match(idn, hz.names)

      # check to make sure there is no overlap in proposed site + hz variable names
      if (any(new.cols %in% hz.names[-ID.idx])) {
        stop('duplicate names in new site / existing horizon data not allowed', call. = FALSE)
      }
      
      # existing site data (may be absent == 0-row data.frame)
      s <- object@site
      
      # join to existing data: by default it will only be on idname(object)
      
      # LEFT JOIN
      site.new <- merge(s, value, all.x = TRUE, sort = FALSE)
      
      new.id.order <- site.new[[idn]]
      if (length(new.id.order) != length(pid) || any(new.id.order != pid)) {
        # join condition resulted in sorting of sites, re-apply original order
        if (any(is.na(pid))) {
          warning("profile IDs derived from horizon data contain NA!", call. = FALSE)
        }
        site.new <- site.new[match(pid, new.id.order), ]
      }

      # sanity check: site + new data should have same number of rows as original
      if (nrow(s) != nrow(site.new)) {
      	message(paste('original data (', nrow(s), ' rows) new data (',
      	              nrow(site.new), ' rows)', sep = ''))
        stop('invalid join condition, site data not changed', call. = FALSE)
      }

      # 2020-05-30: subclasses of data.frame have more than one class
      object@site <- .as.data.frame.aqp(site.new, adf)
	  }

    # done
    return(object)
  }
)

# update an SPC object:
# add site data
# remove named columns from horizons
# return new SPC object
.createSiteFromHorizon <- function(object, mf){

  # create a numeric index for named site columns, as we will remove them
  # from the horizon data
  names_attr <- names(mf)
  idx <- match(names_attr, horizonNames(object))

  # remove the index to the ID column, as we do not want to remove this from
  # the horizon data !
  idx <- idx[-match(idname(object), names_attr)]

  .SD <- NULL
  
  dth <- data.table::as.data.table(horizons(object))
  
  new_site_data <- .as.data.frame.aqp(unique(dth[, .SD, .SDcols = names_attr]), aqp_df_class(object))
  
  if (nrow(new_site_data) != length(object)) {
    warning("One or more horizon columns cannot be normalized to site. Leaving site data unchanged.", call. = FALSE)
    return(object)
  }
  
  # if site data is already present, we don't overwrite/erase it
  site_data <- merge(object@site, new_site_data, by = idname(object), all.x = TRUE, sort = FALSE)

  # remove the named site data from horizon_data
  h <- object@horizons
  hnames <- colnames(h)
  for(i in idx) {
    h[[hnames[i]]] <- NULL
  }

  object@horizons <- .as.data.frame.aqp(h, aqp_df_class(object))

  # replace existing site data
  object@site <- .as.data.frame.aqp(site_data, aqp_df_class(object))

  # done
  return(object)
}


setGeneric('replaceHorizons<-', function(object, value)
  standardGeneric('replaceHorizons<-'))

#' @title Replace Data in Horizon Slot
#'
#' @name replaceHorizons<-
#'
#' @description Replaces horizon data with new `data.frame` object.
#'
#' @param object A `SoilProfileCollection`
#' 
#' @param value An object inheriting `data.frame`
#' 
#' @aliases replaceHorizons<-,SoilProfileCollection-method
#' @docType methods
#'
#' @rdname replaceHorizons
#' @export
#'
#' @examples
#'
#' # load test data
#' data(sp2)
#'
#' # promote to SPC
#' depths(sp2) <- id ~ top + bottom
#'
#' # one profile
#' p <- sp2[1,]
#'
#' # 23 variables in horizon data
#' length(horizonNames(sp2))
#'
#' # remove all but essential ones
#' replaceHorizons(p) <- horizons(p)[,c(idname(p), hzidname(p), horizonDepths(p))]
#'
#' # inspect result (a clean slate)
#' horizons(p)
#'
setReplaceMethod("replaceHorizons",
                 signature(object = "SoilProfileCollection"),
                 function(object, value) {

  hmn <- .hzMetadataNames(object, depths = TRUE)
  required.columns <-  c(idname(object), horizonDepths(object))
  required.missing <- !required.columns %in% names(value)

  if (any(required.missing)) {
    stop(paste0("required horizon data are missing from replacement: ",
                paste0(required.columns[required.missing], 
                       collapse = ", ")), call. = FALSE)
  }
  
  ids.match1 <- all(profile_id(object) %in% value[[idname(object)]])
  if (!ids.match1) {
    stop("profile IDs in site are missing from replacement horizons!", call. = FALSE)
  }
  
  ids.match2 <- all(value[[idname(object)]] %in% profile_id(object))
  if (!ids.match2) {
    stop("profile IDs in replacement are missing from site!", call. = FALSE)
  }
  
  optional.columns <-  hmn[!hmn %in% required.columns]
  optional.missing <- !optional.columns %in% names(value)

  if (any(optional.missing)) {
    message(paste0("optional columns are missing from replacement: ",
                   paste0(optional.columns[optional.missing],
                          collapse = ", ")))
  }

  # assign hzID if hzidname() is missing
  if (optional.missing[1]) {
    value$hzID <- as.character(1:nrow(value))
    hzidname(object) <- "hzID"
    message("no horizon ID present, defaulting to `hzID`")
  }

  object@horizons <- .as.data.frame.aqp(value, aqp_df_class(object))
  return(object)
})


setGeneric('horizons<-', function(object, value)
  standardGeneric('horizons<-'))

#' Join Data to Horizon Slot
#'
#' @name horizons<-
#'
#' @description
#' Horizon data in an object inheriting from \code{data.frame} can easily be added via merge (LEFT JOIN). There must be one or more same-named columns (with at least some matching data) on the left and right hand side to facilitate the join: \code{horizons(spc) <- newdata}
#'
#' @param object A SoilProfileCollection
#' @param value An object inheriting \code{data.frame}
#' @aliases horizons<-,SoilProfileCollection-method
#' @docType methods
#' @export
#' @rdname horizons
#'
#' @examples
#'
#' # load test data
#' data(sp2)
#'
#' # promote to SPC
#' depths(sp2) <- id ~ top + bottom
#'
#' # assign true to surface horizon
#' newdata <- data.frame(top = 0,
#'                       newvalue = TRUE)
#'
#' # do left join
#' horizons(sp2) <- newdata
#'
#' # inspect site table: newvalue TRUE only for horizons
#' #  with top depth equal to zero
#' horizons(sp2)
#'
setReplaceMethod("horizons", signature(object = "SoilProfileCollection"),
  function(object, value) {

  idn <- idname(object)
  hdn <- hzidname(object)
  hzd <- horizonDepths(object)
  
  if (is.null(value))
    stop("new horizon data must not be NULL; to remove a site or horizon attribute use `spc$attribute <- NULL`", call. = FALSE)

  # testing the class of the horizon data to add to the object
  if (!inherits(value, "data.frame"))
    stop("new horizon data input value must inherit from data.frame", call. = FALSE)

  # allow short-circuit (handles NULL and non-op without going thru merge())
  if ((all(horizonNames(object) %in% colnames(value)) ||
       all(colnames(value) %in% horizonNames(object))) &&
       all(c(idn, hdn, hzd) %in% colnames(value)) &&
      nrow(value) == nrow(object)) {
    if (!all(value[[idn]] %in% profile_id(object))) {
      message("Some profile IDs in input data are not present in object and no new columns to merge. Doing nothing.")
      return(object)
    }
    target.order <- order(object@horizons[[idn]], object@horizons[[hzd[1]]])
    input.order <- order(value[[idn]], value[[hzd[1]]])
    idx.order <- match(input.order, target.order)
    # print(idx.order)
    object@horizons <- .as.data.frame.aqp(value, aqp_df_class(object))[idx.order,]
    return(object)
  }

  # get the corresponding vector of IDs, will be used to compute distinct attributes
  ids <- as.character(horizons(object)[[idn]])

  # get column names from proposed horizons, and existing site
  ns <- names(value)
  nh <- siteNames(object)

  # check there is >1 column shared b/w existing horizon and value
  if (length(ns) > 1 && !any(ns %in% horizonNames(object))) {
    stop("new data must have one or more column names in common with the horizon data", call. = FALSE)
  }
  
  ## remove ID column from names(site)
  ID.idx <- match(idn, nh)

  # check to make sure there is no overlap in proposed site + hz variable names
  if (any(ns %in% nh[-ID.idx])) {
    stop('duplicate names in new horizon / existing site data not allowed', call. = FALSE)
  }
  
  h.id <- as.character(object@horizons[[hdn]])
  original.site.order <- match(rle(object@site[[idn]])$values,
                               object@site[[idn]])

  # in keeping with tradition of site<-, we let the join happen
  # left join to existing data
  suppressMessages({
    horizon.new <- merge(object@horizons,
                         value,
                         #by = c(idname(object), hzidname(object)),
                         all.x = TRUE, sort = FALSE)
  })
  
  # TODO: data.table merge() would fix the need for re-sorting, but would check data types
  #       which might cause some backwards compatibility problems (requiring type conversion)
  #       this is a problem because the join is completely open ended -- not just based on ID
  
  chnew <- rle(horizon.new[[idn]])$values
  if (length(chnew) != length(original.site.order) |
     suppressWarnings(any(original.site.order != chnew))) {
    new.horizon.order <- order(horizon.new[[idn]], 
                               horizon.new[[hzd[1]]])
    # message("join condition resulted in sorting of horizons, re-applying original order")
    horizon.new <- horizon.new[new.horizon.order,]
  }

  # sanity check: horizons + new data should have same number of rows as original
  if (nrow(object@horizons) != nrow(horizon.new)) {
    message(paste('original data (', nrow(object@horizons), ' rows) new data (', nrow(horizon.new), ' rows)', sep = ''))
    stop("invalid horizons left join condition, data not changed", call. = FALSE)
  }

  # 2020-05-30: subclasses of data.frame have more than one class
  object@horizons <- .as.data.frame.aqp(horizon.new, aqp_df_class(object))

  # check to make sure same profile IDs are present
  if (any(!(ids %in% as.character(object@horizons[[idn]])))) {
    print(paste('pedons (', nrow(object), ') rows of horizon data (', nrow(object@horizons), ')', sep = ''))
    stop('profile IDs are missing from join result, data not changed', call. = FALSE)
  }

  # done
  return(object)
})

setGeneric('diagnostic_hz<-', function(object, value)
  standardGeneric('diagnostic_hz<-'))

#' @name diagnostic_hz<-
#'
#' @description 
#' 
#'  - `diagnostic_hz<-` (set method): Set diagnostic feature data for a SoilProfileCollection. The profile ID column from `object` (`idname(object)`) must be present in the replacement `value` object.
#' 
#' @param object A SoilProfileCollection
#' @param value An object inheriting from \code{data.frame}
#'
#' @aliases diagnostic_hz<-,SoilProfileCollection-method
#' @docType methods
#' @export
#' @rdname diagnostic_hz
#'
#' @examples
#'
#' # load test data
#' data(sp2)
#'
#' # promote to SPC
#' depths(sp2) <- id ~ top + bottom
#'
#' # assign two profiles a zone related to the mollic epipedon
#' newdata <- data.frame(id = c("hon-1","hon-17"),
#'                       featkind = "fixed-depth surface sample",
#'                       featdept = 0,
#'                       featdepb = 18)
#'
#' # do left join
#' diagnostic_hz(sp2) <- newdata
#'
#' # inspect site table: newvalue TRUE only for horizons
#' #  with top depth equal to zero
#' diagnostic_hz(sp2)
#'
setReplaceMethod("diagnostic_hz",
                 signature(object = "SoilProfileCollection"),
  function(object, value) {

  # get the initial data
  d <- diagnostic_hz(object)

  # get column and ID names
  nm <- names(value)
  idn <- idname(object)
  pIDs <- profile_id(object)

  # testing the class of the new data
  if (!inherits(value, "data.frame"))
    stop("diagnostic horizon data must be a data.frame", call. = FALSE)

  # test for the special case where internally-used functions
  # are copying over data from one object to another, and diagnostic_hz(obj) is a 0-row data.frame
  # short-circut, and return original object
  if (nrow(d) == 0 && nrow(value) == 0)
  	return(object)

  # test to make sure that our common ID is present in the new data
  if (!idn %in% nm)
    stop(paste("diagnostic horizon data are missing pedon ID column: ", idn), call. = FALSE)

  uidm <- unique(value[[idn]]) %in% pIDs
  # warn user if some of the IDs in the candidate data are missing
  if (any(!uidm)) {
    # test to make sure that at least one of the IDS in candidate data are present within SPC
    if (all(!uidm)) {
      warning('candidate diagnostic horizon data have NO matching IDs in target SoilProfileCollection object!', call. = FALSE)
    } else warning('some records in candidate diagnostic horizon data have no matching IDs in target SoilProfileCollection object', call. = FALSE)
  }

  # if data are already present, warn the user
  if (nrow(d) > 0)
  	warning('overwriting existing diagnostic horizon data!', call. = FALSE)
  
  # convert id column to character to match @site
  value[[idn]] <- as.character(value[[idn]])
  
  # copy data over
  object@diagnostic <- .as.data.frame.aqp(value, metadata(object)$aqp_df_class)

  # done
  return(object)
})

setGeneric('restrictions<-', function(object, value)
  standardGeneric('restrictions<-'))

#' @name restrictions<-
#'
#' @description 
#' 
#'  - `restrictions<-` (set method): Set restriction data for a SoilProfileCollection. The profile ID column from `object` (`idname(object)`) must be present in the replacement `value` object.
#' @param object A SoilProfileCollection
#' @param value An data.frame object containing at least a column with name `idname(object)`
#'
#' @aliases restrictions<-,SoilProfileCollection-method
#' @docType methods
#'
#' @rdname restrictions
#' @export
#' @examples
#'
#' # load test data
#' data(sp2)
#'
#' # promote to SPC
#' depths(sp2) <- id ~ top + bottom
#'
#' # assign abrupt textural change to a profile
#' newdata <- data.frame(id = c("hon-21"),
#'                       restrkind = "abrupt textural change",
#'                       restrdep = 46)
#'
#' # do left join
#' restrictions(sp2) <- newdata
#'
#' # inspect site table: newvalue TRUE only for horizons
#' #  with top depth equal to zero
#' restrictions(sp2)
#'
setReplaceMethod("restrictions", signature(object = "SoilProfileCollection"),
                 function(object, value) {

                   # get the initial data
                   d <- restrictions(object)

                   # get column and ID names
                   nm <- names(value)
                   idn <- idname(object)
                   pIDs <- profile_id(object)

                   # testing the class of the new data
                   if (!inherits(value, "data.frame"))
                     stop("restriction data must be a data.frame", call. = FALSE)

                   # test for the special case where internally-used functions
                   # are copying over data from one object to another, and diagnostic_hz(obj) is a 0-row data.frame
                   # short-circuit, and return original object
                   if (nrow(d) == 0 && nrow(value) == 0)
                     return(object)

                   # test to make sure that our common ID is present in the new data
                   if (!idn %in% nm)
                     stop(paste("restriction data are missing pedon ID column: ", idn), call. = FALSE)
                   
                   uidm <- unique(value[[idn]]) %in% pIDs
                   # warn user if some of the IDs in the candidate data are missing
                   if (any(!uidm)) {
                     # test to make sure that at least one of the IDs in candidate data are present within SPC
                     if (all(!uidm)) {
                       warning('restriction data have no matching IDs in target SoilProfileCollection object!', call. = FALSE)
                     } else warning('some records in restriction data have no matching IDs in target SoilProfileCollection object', call. = FALSE)
                   }

                   # if data are already present, warn the user

                   if (nrow(d) > 0)
                     warning('overwriting existing restriction data!', call.=FALSE)
                   
                   # convert id column to character to match @site
                   value[[idn]] <- as.character(value[[idn]])
                  
                   # copy data over
                   object@restrictions <- .as.data.frame.aqp(value, metadata(object)$aqp_df_class)

                   # done
                   return(object)
                 }
)
