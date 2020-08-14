# setters -- functions that substantially modify data.frame slot contents

##
## depths<- setter method - to create AQP objects: sorts based on ID and top depth
##
if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) standardGeneric('depths<-'))


#' @aliases depths<-,SoilProfileCollection-method
#' @rdname depths
setReplaceMethod("depths", signature(object = "SoilProfileCollection"),
	function(object, value) {
		message('This is already a SoilProfilecollection-class object, doing nothing.')
		object
	})

#' Initialize a SoilProfileCollection from a data.frame object
#' @name depths<-
#' @param object An object to promote to SoilProfileCollection (inherits from data.frame)
#' @param value A formula specifying the unique profile ID, top and bottom depth column names
#'
#' @aliases depths<-,data.frame-method
#' @rdname depths
#'
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

##
## initialize SP/SPC objects from a model.frame
##
.initSPCfromMF <- function(data, mf, use_class){
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

  # depths
  depthcols <- c(nm[2], nm[3])

  # enforce numeric depths and provide QC warnings as needed
  data[[depthcols[1]]] <- .checkNAdepths(data[[depthcols[1]]], "top")
  data[[depthcols[2]]] <- .checkNAdepths(data[[depthcols[2]]], "bottom")

  iddata <- data[[nm[1]]]
  tdep <- data[[depthcols[1]]]

  usortid <- unique(sort(iddata))

  idtdepord <- order(as.character(iddata), tdep)
  ditd <- data[idtdepord,]
  hsorttdep <- all(ditd[[depthcols[1]]] == tdep)

  # re-sort horizon data
  if (suppressWarnings(any(iddata != usortid) | !hsorttdep)) {
    ## note: forced character sort on ID -- need to impose some order to check depths
    data <- ditd
  }

  # create a site table with just IDs
  # d'OH need to do this AFTER re-sorting!!!
  nusite <- .as.data.frame.aqp(data.frame(.coalesce.idx(data[[nm[1]]]),
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


##
## initialize site data
##
#' Create or add data to the site slot
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
#'
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
if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, value)
    standardGeneric('site<-'))

setReplaceMethod("site", signature(object = "SoilProfileCollection"),
  function(object, value) {

    # get profile IDs from horizon table
    ids <- as.character(horizons(object)[[idname(object)]])
    ids.coalesce <- .coalesce.idx(ids)

  	# creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, object@horizons, na.action = na.pass)
      nm <- names(mf)
      mf <- data.frame(ids, mf, stringsAsFactors = FALSE)
      names(mf) <- c(idname(object), nm)
      object <- .createSiteFromHorizon(object, mf)
    }

    # creation of site data from an external data.frame via merge (LEFT JOIN)
    if (inherits(value, "data.frame")) {
      # get column names from proposed site, and existing horizons
      ns <- names(value)
      nh <- horizonNames(object)
      
      # site and horizons<- allow short circuiting to ensure that site(x)$<- and horizons(x)$<- work
      if (all(colnames(value) %in% siteNames(object)) &
          idname(object) %in% colnames(value) &
          nrow(value) == length(object)) {  
        
        # re-sorts for case when "joining" only ID e.g. when unioning SPCs with only profile id in @site
        sort.idx <- match(profile_id(object), value[[idname(object)]])
        object@site <- .as.data.frame.aqp(value, aqp_df_class(object))[sort.idx, , drop = FALSE]
        return(object)
      }

      ## remove ID column from names(horizons)
      ID.idx <- match(idname(object), nh)

      # check to make sure there is no overlap in proposed site + hz variable names
      if(any(ns %in% nh[-ID.idx]))
        stop('duplicate names in new site / existing horizon data not allowed', call. = FALSE)

      # existing site data (may be absent == 0-row data.frame)
      s <- object@site

      if(any(s[[idname(object)]] != ids.coalesce)) {
        warning("site and horizon data are out of sync!")
      }

      # join to existing data: by default it will only be idname(object)

      ## an appropriate ID must exist in 'value' AND @site for this to work
      # LEFT JOIN
      suppressMessages(site.new <- merge(s, value, all.x = TRUE, sort = FALSE))

      new.id.order <- site.new[[idname(object)]]
      if(any(new.id.order != ids.coalesce)) {
        message("join condition resulted in sorting of sites, re-applying original order")
        site.new <- site.new[match(ids.coalesce, new.id.order),]
      }

      # sanity check: site + new data should have same number of rows as original
      if(nrow(s) != nrow(site.new)) {
      	message(paste('original data (', nrow(s), ' rows) new data (', nrow(site.new), ' rows)', sep=''))
        stop('invalid join condition, site data not changed', call.=FALSE)
      }

      # 2020-05-30: subclasses of data.frame have more than one class
      object@site <- .as.data.frame.aqp(site.new, metadata(object)$aqp_df_class)
	  }

    ## TODO: finer reporting on what the problem might be
    # check to make sure the the number of rows in @site is the same as length(object)
    if(length(object) != nrow(site(object))){
    	print(paste('pedons (', length(object), ') rows of site data (', nrow(site(object)), ')', sep=''))
    	stop('invalid site data, non-unique values present in horizon data?', call.=FALSE)
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

  # remove the index to the ID columnm, as we do not want to remove this from
  # the horizon data !
  idx <- idx[-match(idname(object), names_attr)]

  # this will break when multiple horizons in the same pedon have different site data!
  # this seems to work fine in all cases, as we keep the ID column
  # and it ensures that the result is in the same order as the IDs
  new_site_data <- ddply(mf, idname(object),
      .fun=function(x) {
	      unique(x[, names_attr, drop = FALSE])
      }
  )

  # if site data is already present, we don't overwrite/erase it
  site_data <- merge(object@site, new_site_data, by = idname(object),
                     all.x = TRUE, sort = FALSE)

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

#' Replace data in the horizon slot
#'
#' @name replaceHorizons<-
#'
#' @description Replaces horizon data with new data.frame object.
#'
#' @param object A SoilProfileCollection
#' @param value An object inheriting \code{data.frame}
#' @aliases replaceHorizons<-,SoilProfileCollection-method
#' @docType methods
#'
#' @rdname replaceHorizons
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
#' replaceHorizons(p) <- horizons(p)[,c(idname(p),hzidname(p),horizonDepths(p))]
#'
#' # inspect result (a clean slate)
#' horizons(p)
#'
if (!isGeneric('replaceHorizons<-'))
  setGeneric('replaceHorizons<-', function(object, value)
    standardGeneric('replaceHorizons<-'))

setReplaceMethod("replaceHorizons",
                 signature(object = "SoilProfileCollection"),
                 function(object, value) {

  required.columns <-  c(idname(object), horizonDepths(object))
  required.missing <- !required.columns %in% names(value)

  if(any(required.missing))
    stop(paste0("required horizon data are missing: ",
         paste0(required.columns[required.missing], collapse=", ")), call. = FALSE)

  ids.match1 <- all(profile_id(object) %in% value[[idname(object)]])
  if(!ids.match1)
    stop("profile IDs in site are missing from replacement horizons!", call. = FALSE)

  ids.match2 <- all(value[[idname(object)]] %in% profile_id(object))
  if(!ids.match2)
    stop("profile IDs in replacement are missing from site!", call. = FALSE)

  optional.columns <-  c(hzidname(object),
                         hzdesgnname(object),
                         hztexclname(object))

  optional.missing <- !optional.columns %in% names(value)

  #if(any(optional.missing))
    #message(paste0("optional columns are missing: ",
    #               paste0(optional.columns[optional.missing],
    #               collapse=", ")))

  # assign hzID if hzidname() is missing
  if(optional.missing[1]) {
    value$hzID <- as.character(1:nrow(value))
    hzidname(object) <- "hzID"
    message("no horizon ID present, defaulting to `hzID`")
  }

  object@horizons <- .as.data.frame.aqp(value, aqp_df_class(object))
  return(object)
})

#' Add data to the horizons slot
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
#'
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
if (!isGeneric('horizons<-'))
  setGeneric('horizons<-', function(object, value)
    standardGeneric('horizons<-'))

setReplaceMethod("horizons", signature(object = "SoilProfileCollection"),
  function(object, value) {

  if (is.null(value))
    stop("new horizon data must not be NULL; to remove a site or horizon attribute use `spc$attribute <- NULL`", call.=FALSE)

  # testing the class of the horizon data to add to the object
  if (!inherits(value, "data.frame"))
	  stop("new horizon data input value must inherit from data.frame", call.=FALSE)

  # allow short-circuit
  if (all(colnames(value) %in% horizonNames(object)) &
      all(c(idname(object), hzidname(object), horizonDepths(object)) %in% colnames(value)) &
      nrow(value) == nrow(object)) {
    object@horizons <- .as.data.frame.aqp(value, aqp_df_class(object))
    return(object)
  }

  # get the corresponding vector of IDs, will be used to compute distinct attributes
  ids <- as.character(horizons(object)[[idname(object)]])

  # get column names from proposed horizons, and existing site
  ns <- names(value)
  nh <- siteNames(object)

  ## remove ID column from names(site)
  ID.idx <- match(idname(object), nh)

  # check to make sure there is no overlap in proposed site + hz variable names
  if(any(ns %in% nh[-ID.idx]))
    stop('horizons left join value contains duplicate names', call.=FALSE)

  h.id <- as.character(object@horizons[[hzidname(object)]])
  original.horizon.order <- 1:length(h.id)
  names(original.horizon.order) <- h.id

  original.site.order <- match(.coalesce.idx(object@site[[idname(object)]]),
                               object@site[[idname(object)]])

  ## debug
  # print(original.order)

  # in keeping with tradition of site<-, we let the join happen
  # left join to existing data
  suppressMessages(horizon.new <- merge(object@horizons,
                                        value,
                                        #by = c(idname(object), hzidname(object)),
                                        all.x = TRUE, sort = FALSE))

  new.horizon.order <- match(names(original.horizon.order),
                             horizon.new[[hzidname(object)]])
  chnew <- .coalesce.idx(horizon.new[[idname(object)]])
  if (length(chnew) != length(original.site.order) |
     suppressWarnings(any(original.site.order != chnew))) {
    # message("join condition resulted in sorting of horizons, re-applying original order")
    horizon.new <- horizon.new[new.horizon.order,]
  }

  # sanity check: horizons + new data should have same number of rows as original
  if(nrow(object@horizons) != nrow(horizon.new)) {
    message(paste('original data (', nrow(object@horizons), ' rows) new data (', nrow(horizon.new), ' rows)', sep=''))
    stop("invalid horizons left join condition, data not changed", call.=FALSE)
  }

  # 2020-05-30: subclasses of data.frame have more than one class
  object@horizons <- .as.data.frame.aqp(horizon.new, aqp_df_class(object))

  # check to make sure same profile IDs are present
  if(any(!(ids %in% as.character(object@horizons[[idname(object)]])))) {
    print(paste('pedons (', nrow(object), ') rows of horizon data (', nrow(object@horizons), ')', sep=''))
    stop('profile IDs are missing from join result, data not changed', call.=FALSE)
  }

  # done
  return(object)
})

#' Add data to the diagnostic slot
#'
#' @name diagnostic_hz<-
#'
#' @description Diagnostic data in an object inheriting from \code{data.frame} can easily be added via merge (LEFT JOIN). There must be one or more same-named columns containing profile ID on the left and right hand side to facilitate the join: \code{diagnostic_hz(spc) <- newdata}
#'
#' @param object A SoilProfileCollection
#' @param value An object inheriting \code{data.frame}
#'
#' @aliases diagnostic_hz<-,SoilProfileCollection-method
#' @docType methods
#'
#' @rdname diagnostic_hz-set
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
if (!isGeneric('diagnostic_hz<-'))
  setGeneric('diagnostic_hz<-', function(object, value)
    standardGeneric('diagnostic_hz<-'))

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
  if(nrow(d) == 0 & nrow(value) == 0)
  	return(object)

  # test to make sure that our common ID is present in the new data
  if(! idn %in% nm)
  	stop(paste("diagnostic horizon data are missing pedon ID column: ", idn), call.=FALSE)

  # test to make sure that at least one of the IDS in candidate data are present within SPC
  if(all( ! unique(value[[idn]]) %in% pIDs) )
  	warning('candidate diagnostic horizon data have NO matching IDs in target SoilProfileCollection object!', call. = FALSE)

  # warn user if some of the IDs in the candidate data are missing
  if(any( ! unique(value[[idn]]) %in% pIDs) ) {
    warning('some records in candidate diagnostic horizon data have no matching IDs in target SoilProfileCollection object')
  }

  # if data are already present, warn the user
  if(nrow(d) > 0)
  	warning('overwriting existing diagnostic horizon data!', call.=FALSE)

  # copy data over
  object@diagnostic <- .as.data.frame.aqp(value, metadata(object)$aqp_df_class)

  # done
  return(object)
})


#' Add data to the restrictions slot
#'
#' @name restrictions<-
#'
#' @description Restrictions data in an object inheriting from \code{data.frame} can easily be added via merge (LEFT JOIN). There must be one or more same-named profile ID columns on the left and right hand side to facilitate the join: \code{restrictions(spc) <- newdata}.
#'
#' @param object A SoilProfileCollection
#' @param value An object inheriting \code{data.frame}
#'
#' @aliases restrictions<-,SoilProfileCollection-method
#' @docType methods
#'
#' @rdname restrictions-set
#'
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
if (!isGeneric('restrictions<-'))
  setGeneric('restrictions<-', function(object, value)
    standardGeneric('restrictions<-'))

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
                     stop("restriction data must be a data.frame", call.=FALSE)

                   # test for the special case where internally-used functions
                   # are copying over data from one object to another, and diagnostic_hz(obj) is a 0-row data.frame
                   # short-circuit, and return original object
                   if(nrow(d) == 0 & nrow(value) == 0)
                     return(object)

                   # test to make sure that our common ID is present in the new data
                   if(! idn %in% nm)
                     stop(paste("restriction data are missing pedon ID column: ", idn), call.=FALSE)

                   # test to make sure that at least one of the IDs in candidate data are present within SPC
                   if(all(!unique(value[[idn]]) %in% pIDs) )
                     warning('restriction data have no matching IDs in target SoilProfileCollection object!', call. = FALSE)

                   # warn user if some of the IDs in the candidate data are missing
                   if(any( ! unique(value[[idn]]) %in% pIDs) ) {
                     warning('some records in restriction data have no matching IDs in target SoilProfileCollection object')
                   }

                   # if data are already present, warn the user
                   if(nrow(d) > 0)
                     warning('overwriting existing restriction data!', call.=FALSE)

                   # copy data over
                   object@restrictions <- .as.data.frame.aqp(value, metadata(object)$aqp_df_class)

                   # done
                   return(object)
                 }
)
