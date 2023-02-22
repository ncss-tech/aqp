#' @title An S4 object representation of a group of soil profiles.
#'
#' @name SoilProfileCollection
#' @export
#'
#' @slot idcol character.
#' @slot hzidcol character.
#' @slot depthcols character.
#' @slot metadata list.
#' @slot horizons data.frame.
#' @slot site data.frame.
#' @slot sp SpatialPoints.
#' @slot diagnostic data.frame.
#' @slot restrictions data.frame.
#' @aliases SoilProfileCollection-class
#' @rdname SoilProfileCollection-class
#' @importFrom sp SpatialPoints proj4string coordinates proj4string<- coordinates<-
#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame
setClass(
  Class = 'SoilProfileCollection',
  representation = representation(
    idcol = 'character', # column name containing IDs
    hzidcol = 'character', # column name containing unique horizon IDs
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
    depthcols = c('top', 'bottom'),
    metadata = list(aqp_df_class = "data.frame", # data.frame subclass
                    aqp_group_by = "",           # grouping variable
                    aqp_hzdesgn = "",            # horizon designation column
                    aqp_texcl = "",              # texture class column
                    stringsAsFactors = FALSE),
    horizons = data.frame(id  = character(0), hzID = character(0),
                          top = numeric(0), bottom = numeric(0),
                          stringsAsFactors = FALSE),
    site = data.frame(id = character(0), stringsAsFactors = FALSE),
    sp = sp::SpatialPoints(data.frame(x = 0, y = 0))[-1, ],
    diagnostic = data.frame(stringsAsFactors = FALSE),
    restrictions = data.frame(stringsAsFactors = FALSE)
  ),
  validity = function(object) {
    # https://github.com/ncss-tech/aqp/issues/75
    return(spc_in_sync(object)$valid)
  }
)

# 2019-03-15: creating an empty SpatialPoints object requires more effort
# c/o: https://gis.stackexchange.com/questions/291069/creating-empty-spatialpoints-or-spatialpointsdataframe-in-r
# old: new('SpatialPoints')
# new: SpatialPoints(data.frame(x = 0, y = 0))[-1,]

#' Constructor for the SoilProfileCollection object
#'
#' @param idcol character Profile ID Column Name
#' @param hzidcol character Horizon ID Column Name
#' @param depthcols character, length 2 Top and Bottom Depth Column Names
#' @param metadata list, metadata including data.frame class in use and depth units
#' @param horizons data.frame An object inheriting from data.frame containing Horizon data.
#' @param site data.frame An object inheriting from data.frame containing Site data.
#' @param sp SpatialPoints A SpatialPoints object. No longer used in aqp 2+, see `?initSpatial`
#' @param diagnostic data.frame An object inheriting from data.frame containing diagnostic feature data. Must contain profile ID. See \code{diagnostic_hz()}
#' @param restrictions data.frame An object inheriting from data.frame containing restrictive feature data. Must contain profile ID. See \code{restrictions()}
#'
#' @description In general, one should use \code{depths()} to initiate a SoilProfileCollection object from data. However, sometimes there are instances where either an empty, or very specific, object is needed. If that is the case, the general constructor \code{SoilProfileCollection} is available.
#'
#' @author Pierre Roudier, Dylan E. Beaudette, Andrew G. Brown
#' @export
#'
#' @rdname SoilProfileCollection-class
#' @examples
#'
#' ## structure of default, empty SoilProfileCollection
#' str(SoilProfileCollection())
#'
#'
#' ## use the depths() formula interface to specify
#' ## profile ID, top and bottom depth and set up
#' ## a SPC that is topologically correct and complete
#'
#' d <- do.call('rbind', lapply(1:10, random_profile))
#'
#' # promote to SoilProfileCollection and plot
#' depths(d) <- id ~ top + bottom
#' plot(d)
#'
#' # split into new SoilProfileCollection objects by index
#' d.1 <- d[1, ]
#' d.2 <- d[2, ]
#' d.345 <- d[3:5, ]
#'
#' # combine profile collections
#' # note that profiles are sorted according to ID
#' d.new <- c(d.345, d.1, d.2)
#' plot(d.new)
#'
#' data(sp1)
#'
#' ## init SoilProfileCollection objects from data.frame
#' depths(sp1) <- id ~ top + bottom
#'
#' ## depth units
#' du <- depth_units(sp1)
#' depth_units(sp1) <- 'in'
#' depth_units(sp1) <- du
#'
#' ## horizon designation column
#' hzdesgnname(sp1) <- "name"
#' hzdesgnname(sp1)
#'
#' ## all designations in an SPC (useful for single profile SPC)
#' hzDesgn(sp1)
#'
#' ## horizon texture class column
#' hztexclname(sp1) <- "texture"
#' hztexclname(sp1)
#'
#' ## get/set metadata on SoilProfileCollection objects
#' # this is a 1-row data.frame
#' m <- metadata(sp1)
#' m$sampler <- 'Dylan'
#' metadata(sp1) <- m
#'
#' ## extract horizon data from SoilProfileCollection objects as data.frame
#' h <- horizons(sp1)
#'
#' # also merge (left-join) of new columns and
#' # replacement of existing columns via horizons<-
#' horizons(sp1) <- h
#'
#' # get number of horizons
#' nrow(sp1)
#'
#'
#' ## getting site-level data
#' site(sp1)
#'
#' ## setting site-level data
#' # site-level data from horizon-level data (stored in @horizons)
#' site(sp1) <- ~ group
#'
#'
#' # make some fake site data, and append from data.frame
#' # a matching ID column must be present in both @site and new data
#' # note that IDs should all be character class
#' d <- data.frame(id=profile_id(sp1), p=runif(n=length(sp1)), stringsAsFactors=FALSE)
#' site(sp1) <- d
#'
#' # edit horizon depths
#' horizonDepths(sp1) <- c('t', 'b')
#' horizonDepths(sp1)
#'
#' # edit profile IDs
#' p <- sprintf("%s-new", profile_id(sp1))
#' profile_id(sp1) <- p
#' profile_id(sp1)
#'
"SoilProfileCollection" <-
  function(idcol = 'id',
           hzidcol = 'hzID',
           depthcols = c('top', 'bottom'),
           metadata = list(aqp_df_class = "data.frame",
                           aqp_group_by = "",
                           aqp_hzdesgn = "",
                           aqp_hztexcl = "",
                           stringsAsFactors = FALSE),
           horizons = data.frame(
             id  = character(0),
             hzID = character(0),
             top = numeric(0),
             bottom = numeric(0),
             stringsAsFactors = FALSE
           ),
           site = data.frame(id = character(0), stringsAsFactors = FALSE),
           sp = new('SpatialPoints'),
           diagnostic = data.frame(stringsAsFactors = FALSE),
           restrictions = data.frame(stringsAsFactors = FALSE)) {

    # retrieve highest-level data.frame subclass of horizon data
    hzclass <- class(horizons)[1]

    new.metadata <- metadata

    # set metadata (default: data.frame, centimeters)
    metadata <- list(
      aqp_df_class = hzclass,
      aqp_group_by = "",
      aqp_hzdesgn = "",
      aqp_hztexcl = "",
      depth_units = 'cm',
      stringsAsFactors = FALSE,

      # calculate data order (original)
      original.order = order(as.character(horizons[[idcol]]),
                             horizons[[depthcols[1]]])
    )

    # the target order to check/maintain is the default for a new SPC
    # metadata$target.order <- metadata$original.order

    # add any custom metadata
    metadata <- c(metadata,
                  new.metadata[!names(new.metadata) %in% names(metadata)])

    # add aqp_group_by if not defined
    if (length(metadata$aqp_group_by) == 0)
      metadata$aqp_group_by <- ""

    # "allow" NULL for the optional slots
    if(length(metadata$aqp_hzdesgn) == 0)
      metadata$aqp_hzdesgn <- ""

    if(length(metadata$aqp_hztexcl) == 0)
      metadata$aqp_hztexcl <- ""

    # create object
    new(
      "SoilProfileCollection",
      idcol = idcol,
      hzidcol = hzidcol,
      depthcols = depthcols,
      metadata = metadata,
      horizons = .as.data.frame.aqp(horizons, hzclass),
      site = .as.data.frame.aqp(site, hzclass),
      sp = sp,
      diagnostic = .as.data.frame.aqp(diagnostic, hzclass),
      restrictions = .as.data.frame.aqp(restrictions, hzclass)
    )
  }

## show
#' SoilProfileCollection show method
#' @name show
#'
#' @description Pretty output method for SoilProfileCollection objects. By default this method limits output to 10 columns and 6 rows from the site and horizon tables respectively.
#'
#' There is an aqp environment option you can set to increase the number of columns shown by default: \code{options(.aqp.show.n.cols = 100)},
#'
#' @param object a SoilProfileCollection
#' @aliases show,SoilProfileCollection-method
#' @docType methods
#' @rdname show
#' @examples
#'
#' # load a SoilProfileCollection
#' data(sp5)
#'
#' # use the show() method
#' show(sp5)
#'
#' # which is same as this (in the console)
#' sp5
#'
setMethod(f = 'show',
          signature(object = 'SoilProfileCollection'),
          function(object) {

            # stats for later
            n.profiles <- length(object)
            n.hz <- nrow(object)

            # count number of hz and site columns for reporting truncated listing
            n.hz.cols <- length(horizonNames(object))
            n.site.cols <- length(siteNames(object))

            # local copies
            s <- object@site
            h <- object@horizons

            # determine number of rows to show
            n.s <- nrow(s)
            n.h <- nrow(h)

            # show up to 6 rows for each slot, or max rows if less
            rows.show.s <- 1:pmin(6, nrow(s))
            rows.show.h <- 1:pmin(6, nrow(h))

            # handle zero row case (so it doesnt fill with NA)
            if (n.h == 0)
              rows.show.h <- 0

            if (n.s == 0)
              rows.show.s <- 0

            # subset rows
            h <- h[rows.show.h, , drop = FALSE]
            s <- s[rows.show.s, , drop = FALSE]

            # move IDs and depths, horizon designation/texture if available
            hzm <- .hzMetadataNames(object, depths = TRUE)
            idx <- match(hzm, names(h))

            # determine number of columns to show, and index to hz / site data
            # user sett-able
            show.cols <- getOption('.aqp.show.n.cols')

            h.n <- names(h)

            # handle empty case
            hz.show <- numeric(0)
            site.show <- numeric(0)

            if (length(h.n) > 0) {

              h <- .data.frame.j(h, c(h.n[idx], h.n[-na.omit(idx)]), aqp_df_class(object))

              # show first n
              hz.show <- seq(from = 1,
                             to = pmin(show.cols, n.hz.cols),
                             by = 1)
              site.show <- seq(from = 1,
                               to = pmin(show.cols, n.site.cols),
                               by = 1)
            }

            # column subseting
            if (length(hz.show) > 0) {
              # generate text explaining truncated summary
              hz.txt <- sprintf(
                "\n----- Horizons (%s / %s rows  |  %s / %s columns) -----\n",
                nrow(h),
                n.h,
                pmin(show.cols, n.hz.cols),
                n.hz.cols
              )
            } else {
              hz.txt <- "[EMPTY]\n"
            }

            if(length(site.show) > 0) {
              site.txt <- sprintf(
                "\n----- Sites (%s / %s rows  |  %s / %s columns) -----\n",
                nrow(s),
                n.s,
                pmin(show.cols, n.site.cols),
                n.site.cols
              )
            } else {
              site.txt <- "[EMPTY]\n"
            }

            # header
            header.txt <-
              sprintf(
                "SoilProfileCollection with %s profiles and %s horizons\nprofile ID: %s  |  horizon ID: %s \nDepth range: %s - %s %s\n",
                n.profiles,
                n.hz,
                idname(object),
                hzidname(object),
                min(object),
                max(object),
                depth_units(object)
              )
            cat(header.txt)

            # make note of additional hz attributes
            cat(hz.txt)

            print(data.frame(h)[, hz.show, drop = FALSE], row.names = FALSE)

            if(n.h > max(rows.show.h))
              cat('[... more horizons ...]\n')

            # make note of additional site attributes
            cat(site.txt)

            print(data.frame(s)[, site.show, drop = FALSE], row.names = FALSE)

            if(n.s > max(rows.show.s))
              cat('[... more sites ...]\n')

            # presence of spatial data
            if (validSpatialData(object)) {
              cat('\nSpatial Data:\n')
              cat("  CRS: ", prj(object), ";", sep = "")
              cat(.spc_bbox(object))
            } else {
              cat('\nSpatial Data:\n[EMPTY]\n')
            }

          })

.spc_bbox <- function(x) {
  crds <- metadata(x)$coordinates
 
  # this bbox outputs for "point" geometries specified in two columns
  # TODO: extend as needed if other geometry types are added (i.e. wrapper around st_bbox())
  if (length(crds) != 2) 
    return("\n")
  
  # if all coordinates in X or Y are NA warnings will be generated & Inf/-Inf result
  suppressWarnings({
    paste0(" ", crds[1], ": ", min(x[[crds[1]]], na.rm = TRUE), " to ", max(x[[crds[1]]], na.rm = TRUE), "; ",
                crds[2], ": ", min(x[[crds[2]]], na.rm = TRUE), " to ", max(x[[crds[2]]], na.rm = TRUE), "\n")
  })
}

#' @description `as.character()`: Character Representation of SoilProfileCollection Object
#' @param x a SoilProfileCollection
#' @param ... additional arguments (not used)
#' @keywords internal
#' @rdname show
#' @export
setMethod('as.character', 'SoilProfileCollection', function(x, ...) {
  paste0('SPC<', length(x), ",", nrow(x), '>')
})

#' @title Wrapper method for data.frame subclass conversion
#' @noRd
#' @param x ANY.
#' @param as.class `"data.frame"`, `"tibble"`, or `"data.table"` default: `"data.frame"`
#' @param ... Additional arguments to coercion function  `as.data.frame`, `as_tibble` or `as.data.table`
#' @return a subclass of `data.frame` corresponding to `as.class`
.as.data.frame.aqp <- function(x, as.class = "data.frame", ...) {
  # 2020-05-30: sub-classes of data.frame have more than one class

  # NULL x -- probably from unusual use cases
  if (class(x)[1] == "NULL")
    stop(sprintf("input object is NULL, expected '%s'", as.class))

  # don't invoke coercion methods if not needed
  if (!inherits(x, 'data.frame')) {
    stop(sprintf(
      "input data class %s does not inherit from `data.frame`",
      class(x)[1]
    ),
    call. = TRUE)
  }

  # note: we handle the possibly NULL/0-length as.class
  #       by letting it fall through default switch EXPR
  #       a warning is generated for non-data.frames
  cond <- class(x)[1] == as.class
  test <- all(length(cond) > 0 & cond)

  # this happens if a SPC has had its metadata entry wiped out or old SPC object in Rda file
  if (is.null(test) | is.na(test)) {
    as.class <- "data.frame"
    message(
      "missing metadata for aqp_df_class -- run aqp::rebuildSPC(object) to fix slots and metadata"
    )
  } else if (test) {
    # rm rownames in slots
    rownames(x) <- NULL

    return(x)
  }

  switch(as.class,
         'data.table' = {
           #print(as.class)
           if (requireNamespace("data.table", quietly = TRUE))
             return(data.table::as.data.table(x, ...))
           message(
             "using data.table class in SoilProfileCollection slots requires the `data.table` package"
           )
         },
         'tbl_df' = {
           #print(as.class)
           if (requireNamespace("tibble", quietly = TRUE))
             return(tibble::as_tibble(x, ...))
           message("using tbl_df class in SoilProfileCollection slots requires the `tibble` package")
         },
         {
           # default/data.frame
           #  if we were supposed to get something else,
           #  make a warning with a stack trace
           if (as.class != "data.frame") {
             message(sprintf("failed to use %s as data.frame class", as.class))
             metadata(x)$aqp_df_class <- "data.frame"
             warning(
               "data.table and tbl_df in SoilProfileCollection data.frame slots are EXPERIMENTAL, defaulting to data.frame",
               call. = FALSE
             )
           }

           # return data.frame no matter what
           res <- as.data.frame(x, ...)

           # rm rownames in slots
           rownames(res) <- NULL

           return(res)
         })
}


# basic wrapper function for multi-j index subsetting of data.frames compatible with data.table

.data.frame.j <- function(x, col.names, use_class = class(x)[1]) {

  # see: https://github.com/ncss-tech/aqp/issues/176
  .SD <- NULL

  if (inherits(x, 'data.table')) {
    res <- x[, .SD, .SDcols = col.names]
  } else {
    res <- x[, col.names, drop = FALSE]
  }

  if (inherits(res, 'data.frame')) {
    h <- .as.data.frame.aqp(res, use_class)
    return(h)
  } else {
    stop(".data.frame.j: result does not inherit from `data.frame`", call. = FALSE)
  }
}

setGeneric('hzID<-', function(object, value)
  standardGeneric('hzID<-'))

#' Set horizon IDs
#'
#' @description Set vector containing horizon IDs
#'
#' @param object a SoilProfileCollection
#' @param value a unique vector of equal length to number of horizons \code{nrow(object)}
#' @aliases hzID<-
#' @docType methods
#' @export
#' @rdname hzID
setReplaceMethod("hzID", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   if (!inherits(value, 'character')) {
                     message("converting horizon IDs from integer to character")
                     value <- as.character(value)
                   }

                   # can't be missing
                   if (is.null(value)) {
                     stop('horizon IDs cannot be NULL or NA', call. = FALSE)
                   }

                   if (any(is.na(value)) | any(is.null(value))) {
                     stop('horizon IDs cannot be NULL or NA', call. = FALSE)
                   }

                   # length
                   if (length(value) != nrow(object)) {
                     stop('replacement horizon IDs must have same length as original',
                          call. = FALSE)
                   }

                   # unique
                   if (length(value) != length(unique(value))) {
                     stop('replacement horizon IDs must be unique', call. = FALSE)
                   }

                   # extract horizon and replace IDs
                   object@horizons[[hzidname(object)]] <- value

                   return(object)
                 })

setGeneric('profile_id<-', function(object, value)
  standardGeneric('profile_id<-'))

#' Set profile IDs
#'
#' @name profile_id<-
#'
#' @description Set vector containing profile IDs
#'
#' @param object a SoilProfileCollection
#' @param value a unique vector of equal length to number of profiles \code{length(object)}
#' @aliases profile_id<-,SoilProfileCollection-method
#' @docType methods
#' @rdname profile_id
#' @export
setReplaceMethod("profile_id", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # can't be missing
                   if (is.null(value)) {
                     stop('profile IDs cannot be NULL or NA', call. = FALSE)
                   }

                   if (any(is.na(value)) | any(is.null(value))) {
                     stop('profile IDs cannot be NULL or NA', call. = FALSE)
                   }

                   # length
                   if (length(value) != length(profile_id(object))) {
                     stop('replacement profile IDs must have same length as original',
                          call. = FALSE)
                   }

                   # unique
                   if (length(value) != length(unique(value))) {
                     stop('replacement profile IDs must be unique', call. = FALSE)
                   }

                   # lookup table for converting old -> new IDs
                   idn <- idname(object)
                   pids <- profile_id(object)
                   lut <- cbind(pids, value)

                   # change @site
                   s <- site(object)
                   s[[idn]] <- value
                   object@site <-
                     .as.data.frame.aqp(s, metadata(object)$aqp_df_class)

                   # change @horizons
                   h <- object@horizons
                   update.idx <- match(h[[idn]], lut[, 1])
                   # apply edits via LUT
                   h[[idn]] <- lut[update.idx, 2]
                   object@horizons <-
                     .as.data.frame.aqp(h, metadata(object)$aqp_df_class)

                   # search in @diagnostic
                   dg <- diagnostic_hz(object)
                   dg.nm <- names(dg)
                   idx <- grep(idn, dg.nm)

                   if (length(idx) > 0) {
                     # apply edits via LUT
                     update.idx <- match(dg[[idx]], lut[, 1])
                     dg[[idx]] <- lut[update.idx, 2]
                     suppressWarnings(diagnostic_hz(object) <- dg)
                   }

                   # search in @restrictions
                   re <- restrictions(object)
                   re.nm <- names(re)
                   idx <- grep(idn, re.nm)

                   if (length(idx) > 0) {
                     # apply edits via LUT
                     update.idx <- match(re[[idx]], lut[, 1])
                     re[[idx]] <- lut[update.idx, 2]
                     suppressWarnings(restrictions(object) <- re)
                   }

                   return(object)
                 }
)

setGeneric('horizonDepths<-', function(object, value)
  standardGeneric('horizonDepths<-'))

#' Set horizon depth column names
#'
#' @name horizonDepths<-
#'
#' @description Set column name containing horizon ID
#'
#' @param object a SoilProfileCollection
#' @param value a character vector of length two with names of columns containing numeric top and bottom depths
#' @aliases horizonDepths<-,SoilProfileCollection-method
#' @docType methods
#' @rdname horizonDepths
#' @export
setReplaceMethod("horizonDepths", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # can't be missing
                   if (is.null(value)) {
                     stop('cannot assign NA or NULL depth column names', call. = FALSE)
                   }

                   if (any(is.na(value)) | any(is.null(value))) {
                     stop('cannot assign NA or NULL depth column names', call. = FALSE)
                   }

                   # length
                   if (length(value) != 2) {
                     stop('horizon depth names must be a vector with two items', call. = FALSE)
                   }

                   # warn about changes in names
                   if (any(value != make.names(value))) {
                     warning('names have been modified to legal data.frame column names')
                   }

                   # must be safely convertable to character and safe for DF
                   value <- make.names(value)

                   # save old values
                   hd <- horizonDepths(object)

                   # change @horizons, just the names
                   hn <- horizonNames(object)
                   idx <- match(hd, hn)
                   hn[idx] <- value
                   horizonNames(object) <- hn

                   # change @depthcols
                   object@depthcols <- value

                   return(object)
                 })


setGeneric('horizonNames<-', function(object, value)
  standardGeneric('horizonNames<-'))
  
#' Set horizon column names
#'
#' @name horizonNames<-
#'
#' @description Set horizon column names
#'
#' @param object a SoilProfileCollection
#' @param value a unique vector of equal length to number of columns in horizons \code{length(horizonNames(object))}
#' @aliases horizonNames<-,SoilProfileCollection-method
#' @docType methods
#' @rdname horizonNames
#' @export
setReplaceMethod("horizonNames", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # sanity check
                   if (any(is.null(value)))
                     stop('cannot assign NA or NULL column names', call. = FALSE)

                   if (any(is.na(value)))
                     stop('cannot assign NA or NULL column names', call. = FALSE)

                   # must be same length
                   if (length(value) != length(horizonNames(object))) {
                     stop('replacement must have same length as original', call. = FALSE)
                   }

                   # warn about changes in names
                   if (any(value != make.names(value))) {
                     warning('names have been modified to legal data.frame column names')
                   }

                   # assign
                   names(object@horizons) <- make.names(value)
                   return(object)
                 })

setGeneric('siteNames<-', function(object, value)
  standardGeneric('siteNames<-'))

#' Set site column names
#'
#' @name siteNames<-
#'
#' @description Set site column names
#'
#' @param object a SoilProfileCollection
#' @param value a unique vector of equal length to number of columns in site: \code{length(siteNames(object))}
#' @aliases siteNames<-,SoilProfileCollection-method
#' @docType methods
#' @rdname siteNames
#' @export
setReplaceMethod("siteNames",
                signature(object = "SoilProfileCollection"),
                 function(object, value) {

                   # sanity check
                   if (is.na(value) | is.null(value))
                     stop('cannot assign NA or NULL column names', call. = FALSE)

                   names(object@horizons) <- make.names(value)
                   return(object)
                 })


setGeneric('hzidname<-', function(object, value)
  standardGeneric('hzidname<-'))

#' Set horizon ID column name
#'
#' @name hzidname<-
#'
#' @description Set unique horizon ID column name
#'
#' @param object a SoilProfileCollection
#' @param value character, column name containing unique horizon ID values
#' @aliases hzidname<-,SoilProfileCollection-method
#' @docType methods
#' @rdname hzidname
#' @export
#' @examples
#' data(sp1)
#'
#' # promote to SPC
#' depths(sp1) <- id ~ top + bottom
#'
#' # create new horizon ID
#' sp1$hzIDrev <- rev(sp1$hzID)
#'
#' # set horizon designation column
#' hzidname(sp1) <- "hzIDrev"
#'
#' # get horizon designation column
#' hzidname(sp1)
#'
setReplaceMethod("hzidname",
                signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # quick sanity check
                   if (length(value) != 1)
                     stop("horizon ID name should have length of 1", call. =
                            TRUE)


                   # sanity checks

                   # test: does it exist?
                   if (!value %in% horizonNames(object)) {
                     stop("horizon ID name not in horizon data", call. = TRUE)
                   }

                   # test: unique?
                   x <- object@horizons[[value]]
                   if (length(unique(x)) != nrow(object)) {
                     # convert error to warning,
                     # prevent stoppage from nonunique,
                     # fail gracefully and retain to default
                     warning("horizon ID name (",
                             value,
                             ") not unique. unique ID not changed.",
                             call. = TRUE)
                   } else {
                     # replace
                     object@hzidcol <- value

                     # convert contents to character, if needed
                     if (!is.character(x)) {
                       message(sprintf("converting horizon IDs in column `%s` to character", value))
                       object@horizons[[value]] <-
                         as.character(object@horizons[[value]])
                     }
                   }

                   # done
                   return(object)
                 })

##
## accessors
##


setGeneric("idname", function(object, ...)
  standardGeneric("idname"))

#' Get profile ID column name
#'
#' @description Get column name containing unique profile IDs
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases idname
#' @rdname idname
#' @export
setMethod("idname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@idcol))

setGeneric("hzidname", function(object, ...)
  standardGeneric("hzidname"))

#' Get horizon ID column name
#'
#' @description Get column name containing unique horizon ID
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases hzidname
#' @rdname hzidname
#' @export
setMethod("hzidname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@hzidcol))


setGeneric("hzID", function(object)
  standardGeneric("hzID"))

#' Get horizon IDs
#'
#' @description Get vector containing horizon IDs
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases hzID
#' @rdname hzID
#' @export
setMethod("hzID", signature(object = "SoilProfileCollection"),
          function(object) {
            return(object@horizons[[hzidname(object)]])
          })

## distinct profile IDs
setGeneric("profile_id", function(object)
  standardGeneric("profile_id"))

## relies on ordering of profile IDs in horizons matching site

#' Get/set unique (sorted) profile IDs
#'
#' @description Get or set a vector of profile IDs
#'
#' @param object a SoilProfileCollection
#'
#' @docType methods
#' @aliases profile_id
#' @rdname profile_id
#' @export
setMethod("profile_id", signature(object = "SoilProfileCollection"),
          function(object)
            unique(as.character(object@horizons[[idname(object)]])))

setGeneric("horizonDepths", function(object)
  standardGeneric("horizonDepths"))

#' Get horizon depth column names
#'
#' @description Get column names containing horizon depths
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases horizonDepths
#' @rdname horizonDepths
#' @export
setMethod("horizonDepths", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@depthcols))

## site data
setGeneric("site", function(object, ...)
  standardGeneric("site"))

#' Retrieve site data from SoilProfileCollection
#'
#' @description Get site data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases site
#' @rdname site
#' @export
setMethod("site", signature(object = "SoilProfileCollection"),
          function(object) {
            return(.as.data.frame.aqp(object@site, aqp_df_class(object)))
          })


## diagnostic horizons: stored as a data.frame
setGeneric("diagnostic_hz", function(object, ...)
  standardGeneric("diagnostic_hz"))

#' Retrieve diagnostic data from SoilProfileCollection
#'
#' @description Get diagnostic feature data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#'
#' @docType methods
#' @aliases diagnostic_hz
#' @rdname diagnostic_hz
#' @export
setMethod(f = 'diagnostic_hz', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@diagnostic, aqp_df_class(object)))
          })


## restrictions: stored as data.frame
setGeneric("restrictions", function(object, ...)
  standardGeneric("restrictions"))

#' Retrieve restriction data from SoilProfileCollection
#'
#' @description Get restriction data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases restrictions
#' @rdname restrictions
#' @export
setMethod(f = 'restrictions', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@restrictions, aqp_df_class(object)))
          })

## horizon data
# returns a data.frame with horizons data
setGeneric("horizons", function(object, ...)
  standardGeneric("horizons"))

#' Retrieve horizon data from SoilProfileCollection
#'
#' @description Get horizon data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases horizons
#' @rdname horizons
#' @export
setMethod(f = 'horizons', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@horizons, metadata(object)$aqp_df_class))
          })

## metadata
# returns a data.frame
setGeneric("metadata", function(object, ...)
  standardGeneric("metadata"))

#' Retrieve metadata from SoilProfileCollection
#'
#' @description Get metadata from SoilProfileCollection. Result is a list. Two entries (aqp_df_class, depth_units) should not be edited in the metadata list directly. There are methods that facilitate changing them -- and propagating their changes throughout the collection. Otherwise, metadata list is a free-form slot used to store arbitrary information about the data, how it was collected, citations, etc.
#'
#' @param object a SoilProfileCollection
#' @export
#' @docType methods
#' @aliases metadata
#' @rdname metadata
#' @export
setMethod(f = 'metadata', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(object@metadata)
          })

setGeneric("aqp_df_class", function(object)
  standardGeneric("aqp_df_class"))

#' Get aqp_df_class entry from metadata or return a safe value.
#'
#' @description This is an accessor and replacement method for the \code{aqp_df_class} entry in the metadata slot. This entry is used internally by methods that interact with \code{data.frame} objects and slots to ensure that the same class used to promote to the SoilProfileCollection initially is used throughout the process.
#'
#' @param object a SoilProfileCollection
#' @aliases aqp_df_class
#' @docType methods
#' @aliases aqp_df_class
#' @rdname aqp_df_class
#' @export
setMethod(f = 'aqp_df_class', signature(object = 'SoilProfileCollection'),
          function(object) {
            u <- as.character(metadata(object)[['aqp_df_class']])

            # all handles the logical(0) for undefined
            if (all(u == '')) {
              message("aqp_df_class metadata entry not found - run aqp::rebuildSPC() to fix")
              return("data.frame")
            }

            return(u)
          })

setGeneric("aqp_df_class<-", function(object, value)
  standardGeneric("aqp_df_class<-"))

#' @param value "data.frame", "data.table" or "tbl_df"
#' @aliases aqp_df_class<-
#' @rdname aqp_df_class
#' @export
setReplaceMethod("aqp_df_class", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   if (length(value) != 1 | !is.character(value))
                     stop("aqp_df_class metadata entry must be `data.frame`, `data.table` or `tbl_df`",  call.=FALSE)
                   if (value %in% c("tbl","tibble"))
                     value <- "tbl_df"
                   if (!value %in% c('data.frame',"data.table","tbl_df"))
                     stop("aqp_df_class metadata entry must be `data.frame`, `data.table` or `tbl_df`",  call.=FALSE)
                   metadata(object)$aqp_df_class <- value
                   return(object)
                 })

setGeneric("depth_units", function(object, ...)
  standardGeneric("depth_units"))

#' Get depth units from metadata
#'
#' @description Get units of depth measurement from metadata. Default value is centimeters.
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases depth_units
#' @rdname depth_units
#' @export
setMethod(f = 'depth_units', signature(object = 'SoilProfileCollection'),
          function(object) {
            u <- as.character(metadata(object)[['depth_units']])
            # give a warning if not defined
            if (length(u) == 0 | u == '')
              message('depth_units have not been defined')
            return(u)
          })

setGeneric("siteNames", function(object, ...)
  standardGeneric("siteNames"))

#' Get names of columns in site table
#'
#' @description Get names of columns in site table.
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases siteNames
#' @rdname siteNames
#' @export
setMethod("siteNames", signature(object = "SoilProfileCollection"),
          function(object) {
            res <- names(object@site)
            return(res)
          })

setGeneric("horizonNames", function(object, ...)
  standardGeneric("horizonNames"))

#' Get names of columns in horizon table
#'
#' @description Get names of columns in horizon table.
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases horizonNames
#' @rdname horizonNames
#' @export
setMethod("horizonNames", signature(object = "SoilProfileCollection"),
          function(object) {
            res <- names(object@horizons)
            return(res)
          })

.hzMetadataNames <- function(object, depths = FALSE, ...) {
  hzd <- character(0)
  if (depths) {
    hzd <- horizonDepths(object)
  }
  idn <- c(idname(object),
    hzidname(object),
    hzd,
    hzdesgnname(object),
    GHL(object),
    hztexclname(object))
  idn[!is.na(idn) & nchar(idn) > 0]
}

setGeneric("hzMetadata", function(object, ...)
  standardGeneric("hzMetadata"))

#' Get horizon-level metadata
#'
#' Get `idname(object)` and `hzidname(object)`, with `hzdesgnname(object)`, `hztexclname(object)` (if defined)
#'
#' @param object a SoilProfileCollection
#' @docType methods
#' @aliases hzMetadata
#' @rdname hzMetadata
#' @export
setMethod("hzMetadata", signature(object = "SoilProfileCollection"), 
          function(object) {
            idn <- .hzMetadataNames(object)
            .data.frame.j(horizons(object),
                          col.names = idn[idn %in% horizonNames(object)],
                          use_class = aqp_df_class(object))
          })

##
## initialize metadata: object modification in-place
##

setGeneric('metadata<-', function(object, value)
  standardGeneric('metadata<-'))

#' Set Metadata for a SoilProfileCollection
#'
#' @param object A SoilProfileCollection
#' @param value A named list (see examples)
#' @aliases metadata<-
#' @rdname metadata
#' @export
#' @examples
#' 
#' data(sp5)
#'
#' # replace default metadata with itself
#' metadata(sp5) <- metadata(sp5)
#'
#' # set new metadata attribute value
#' metadata(sp5)$newvalue <- 'foo'
#'
#' # get metadata attribute
#' metadata(sp5)$newvalue
#'
setReplaceMethod("metadata", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # otherwise assign
                   object@metadata <- value
                   
                   # done
                   return(object)
                 })

##
## initialize depth_units: object modification in-place, depth_units stored in @metadata
##
setGeneric('depth_units<-', function(object, value)
  standardGeneric('depth_units<-'))

#' Set units of measurement for profile depth
#'
#' @param object A SoilProfileCollection
#' @param value character, a value representing units. Default \code{'cm'}.
#' @aliases depth_units<-
#' @rdname depth_units
#' @export
#' @examples
#'
#' data(sp5)
#'
#' ## get depth units
#' du <- depth_units(sp5)
#'
#' # set alternate units; e.g. inches
#' depth_units(sp5) <- 'in'
#'
#' # replace original value (cm)
#' depth_units(sp5) <- du
setReplaceMethod("depth_units", signature(object = "SoilProfileCollection"),
                 function(object, value) {

                   # quick sanity check: character, length 1

                   # keep existing metadata
                   md <- metadata(object)

                   # default depth_units are always in metadata
                   # replace what ever is there
                   md$depth_units <- value

                   # replace metadata
                   metadata(object) <- md

                   # done
                   return(object)
                 }
)
