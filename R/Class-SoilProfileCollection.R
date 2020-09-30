#' @title An S4 object representation of a group of soil profiles.
#'
#' @name SoilProfileCollection
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
#'
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
    sp = SpatialPoints(data.frame(x = 0, y = 0))[-1, ],
    diagnostic = data.frame(stringsAsFactors = FALSE),
    restrictions = data.frame(stringsAsFactors = FALSE)
  ),
  validity = function(object) {
    # https://github.com/ncss-tech/aqp/issues/75
    return(spc_in_sync(object)$valid)
  }
)

# 2020-07-10: allows for data.table @Suggests without importing
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
.datatable.aware <-  TRUE

# 2020-05-30: make data.table, tbl_df and data.frame slots "co-exist"
# see: https://stackoverflow.com/questions/35642191/tbl-df-with-s4-object-slots
if(requireNamespace("data.table", quietly = TRUE))
  setOldClass(c("data.table", "data.frame"))

if(requireNamespace("tibble", quietly = TRUE))
  setOldClass(c("tbl_df", "tbl", "data.frame"))

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
#' @param sp SpatialPoints A SpatialPoints object. Generally initialized with \code{coordinates(spc) <- ~ x + y}.
#' @param diagnostic data.frame An object inheriting from data.frame containing diagnostic feature data. Must contain profile ID. See \code{diagnostic_hz()}
#' @param restrictions data.frame An object inheriting from data.frame containing restrictive feature data. Must contain profile ID. See \code{restrictions()}
#'
#' @description In general, one should use \code{depths()} to initiate a SoilProfileCollection object from data. However, sometimes there are instances where either an empty, or very specific, object is needed. If that is the case, the general constructor \code{SoilProfileCollection} is available.
#'
#' @author Pierre Roudier, Dylan E. Beaudette, Andrew G. Brown
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
#' d <- do.call('rbind',lapply(1:10, random_profile))
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
#' # recombine, note that profiles are sorted according to ID
#' d.new <- pbindlist(list(d.345, d.1, d.2))
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

            # move IDs and depths, horizon designation if available
            hzd <- hzdesgnname(object)
            if (hzd != "") {

              # registered in a slot
              idx <- match(c(idname(object),
                             hzidname(object),
                             horizonDepths(object),
                             hzdesgnname(object)), names(h))
            } else {
              # undefined
              idx <- match(c(idname(object), hzidname(object), horizonDepths(object)), names(h))
            }

            # determine number of columns to show, and index to hz / site data
            # user sett-able
            show.cols <- getOption('.aqp.show.n.cols')

            h.n <- names(h)

            # handle empty case
            hz.show <- numeric(0)
            site.show <- numeric(0)

            if (length(h.n) > 0) {

              # aqp:::.data.frame.j is the safe way to use the j index the data.frame way
              #  if you might encounter a data.table
              h <- .data.frame.j(h, c(h.n[idx], h.n[-na.omit(idx)]), aqp_df_class(object))

              # # if defined, move horizon designation to the 3rd column
              # # missing horizon designation evaluates to character(0)
              hzd <- hzdesgnname(object)
              if (hzd != "") {
                idx <- match(hzd, names(h))
                if (length(idx))
                  h <- .data.frame.j(h, c(names(h)[idx], names(h)[-idx]), aqp_df_class(object))
              }

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

            # note: use of the j index here is not compatible with data.table
            #  no need to use aqp:::.data.frame.j here -- this is just for output
            print(.as.data.frame.aqp(data.frame(h)[, hz.show, drop = FALSE],
                                     aqp_df_class(object)), row.names = FALSE)

            if(n.h > max(rows.show.h))
              cat('[... more horizons ...]\n')

            # make note of additional site attributes
            cat(site.txt)

            # again: use of the j index here is not compatible with data.table
            print(.as.data.frame.aqp(data.frame(s)[, site.show, drop = FALSE],
                                     aqp_df_class(object)), row.names = FALSE)

            if(n.s > max(rows.show.s))
              cat('[... more sites ...]\n')

            # presence of spatial data
            if (nrow(coordinates(object)) == n.profiles) {
              cat('\nSpatial Data:\n')
              show(object@sp@bbox)
              show(proj4string(object))
            } else {
              cat('\nSpatial Data: [EMPTY]\n')
            }

          })



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

            # NULL x -- probably from unusual use cases
            if (class(x)[1] == "NULL")
              stop(sprintf("input object is NULL, expected '%s'", as.class))

            # don't invoke coercion methods if not needed
            if (!inherits(x, 'data.frame')) {
              stop(sprintf("input data class %s does not inherit from `data.frame`", class(x)[1]), call. = TRUE)
            }

            # note: we handle the possibly NULL/0-length as.class
            #       by letting it fall through default switch EXPR
            #       a warning is generated for non-data.frames
            cond <- class(x)[1] == as.class
            test <- all(length(cond) > 0 & cond)

            # this happens if a SPC has had its metadata entry wiped out or old SPC object in Rda file
            if (is.null(test) | is.na(test)) {
              as.class <- "data.frame"
              message("missing metadata for aqp_df_class -- run aqp::rebuildSPC(object) to fix slots and metadata")
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
                  metadata(object)$aqp_df_class <- "data.frame"
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


# basic wrapper function for multi-j index subsetting of data.frames compatible with data.table
.data.frame.j <- function(dat, col.names, use_class) {
  dfnames <- names(dat)
  
  # allow for re-ordering by column name like data.frame[,j] 
  dfnamesub <- dfnames[dfnames %in% col.names]
  dfnamesub <- dfnamesub[match(col.names, dfnamesub)]
  
  # access columns one by one in desired order, using "ambivalent" [[ 
  res <- lapply(dfnamesub, function(new.name) {
     newcol <- data.frame(dat[[new.name]], stringsAsFactors = FALSE)
     names(newcol) <- new.name
     return(newcol)
  })
  
  # recombine
  res <- do.call('cbind', res)
  if (inherits(res, 'data.frame')) {
   h <- .as.data.frame.aqp(res, use_class)
   return(h)
  } else {
    # return data unchanged if not inheriting from data.frame
    return(dat)
  }
}


#' Set horizon IDs
#'
#' @name hzID<-
#'
#' @description Set vector containing horizon IDs
#'
#' @param object a SoilProfileCollection
#' @param value a unique vector of equal length to number of horizons \code{nrow(object)}
#'
#' @aliases hzID<-,SoilProfileCollection-method
#' @docType methods
#' @rdname hzID-set
#'
if (!isGeneric('hzID<-'))
  setGeneric('hzID<-', function(object, value)
    standardGeneric('hzID<-'))

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

## profile IDs
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
#'
if (!isGeneric('profile_id<-'))
  setGeneric('profile_id<-', function(object, value)
    standardGeneric('profile_id<-'))

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

## horizon depth columns
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
#'

if (!isGeneric('horizonDepths<-'))
  setGeneric('horizonDepths<-', function(object, value)
    standardGeneric('horizonDepths<-'))

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
#'
if (!isGeneric('horizonNames<-'))
  setGeneric('horizonNames<-', function(object, value)
    standardGeneric('horizonNames<-'))

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
#'
if (!isGeneric('siteNames<-'))
  setGeneric('siteNames<-', function(object, value)
    standardGeneric('siteNames<-'))

setReplaceMethod("siteNames",
                signature(object = "SoilProfileCollection"),
                 function(object, value) {

                   # sanity check
                   if (is.na(value) | is.null(value))
                     stop('cannot assign NA or NULL column names', call. = FALSE)

                   names(object@horizons) <- make.names(value)
                   return(object)
                 })

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

if (!isGeneric('hzidname<-'))
  setGeneric('hzidname<-', function(object, value)
    standardGeneric('hzidname<-'))

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

#' Set horizon designation column name
#' @name hzdesgnname<-
#' @description Set horizon designation column name
#'
#' @param object a SoilProfileCollection
#' @param value character, name of column containing horizon designations
#' @docType methods
#' @aliases hzdesgnname<-,SoilProfileCollection-method
#' @rdname hzdesgnname-set
#' @examples
#'
#' data(sp1)
#'
#' # promote to SPC
#' depths(sp1) <- id ~ top + bottom
#'
#' # set horizon designation column
#' hzdesgnname(sp1) <- "name"
#'
#' # get horizon designation column
#' hzdesgnname(sp1)

if (!isGeneric('hzdesgnname<-'))
  setGeneric('hzdesgnname<-', function(object, value)
    standardGeneric('hzdesgnname<-'))

setReplaceMethod("hzdesgnname",
                signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # test: does it exist?
                   if(!length(value))
                     value <- ""

                   if(length(value)) {
                     # several ways to "reset" the hzdesgnname
                     if((value == "") | is.na(value) | is.null(value)) {
                       value <- ""
                       # message("set horizon designation name column to `character` of length zero")
                     } else if (!(value %in% horizonNames(object))) {
                       stop(paste0("horizon designation name (",value,") not in horizon data"), call.=FALSE)
                     }
                   }

                   # replace
                   metadata(object)$aqp_hzdesgn <- value

                   # done
                   return(object)
                 })

#' Set horizon texture class column name
#' @name hztexclname<-
#'
#' @description Set horizon texture class column name for a SoilProfileCollection
#'
#' @param object a SoilProfileCollection
#' @param value character, name of column containing horizon texture classes
#'
#' @docType methods
#' @aliases hztexclname<-,SoilProfileCollection-method
#' @rdname hztexclname-set
#'
#' @examples
#'
#' data(sp1)
#'
#' # promote to SPC
#' depths(sp1) <- id ~ top + bottom
#'
#' # set horizon texture class column
#' hztexclname(sp1) <- "texture"
#'
#' # get horizon texture class column
#' hztexclname(sp1)

if (!isGeneric('hztexclname<-'))
  setGeneric('hztexclname<-', function(object, value)
    standardGeneric('hztexclname<-'))

setReplaceMethod("hztexclname", signature(object = "SoilProfileCollection"),
                 function(object, value) {
                   # test: does it exist?
                   if(!length(value))
                     value <- ""

                   if(length(value)) {
                     # several ways to "reset" the hzdesgnname
                     if((value == "") | is.na(value) | is.null(value)) {
                       value <- ""
                       #message("set horizon texture class name to `character` of length zero")
                     } else if (! value %in% horizonNames(object)) {
                       stop("horizon texture class name not in horizon data", call.=TRUE)
                     }
                   }

                   # replace
                   metadata(object)$aqp_hztexcl <- value

                   # done
                   return(object)
                 })

##
## accessors
##

#' Get profile ID column name
#'
#' @name idname
#'
#' @description Get column name containing unique profile IDs
#'
#' @param object a SoilProfileCollection
#' @aliases idname,SoilProfileCollection-method
#' @docType methods
#' @rdname idname
#'
if (!isGeneric("idname"))
  setGeneric("idname", function(object, ...)
    standardGeneric("idname"))

setMethod("idname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@idcol))

#' Get horizon ID column name
#'
#' @name hzidname
#'
#' @description Get column name containing unique horizon ID
#'
#' @param object a SoilProfileCollection
#' @aliases hzidname,SoilProfileCollection-method
#' @docType methods
#' @rdname hzidname
#'
if (!isGeneric("hzidname"))
  setGeneric("hzidname", function(object, ...)
    standardGeneric("hzidname"))

setMethod("hzidname", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@hzidcol))

#' Get horizon IDs
#'
#' @name hzID
#' @description Get vector containing horizon IDs
#' @param object a SoilProfileCollection
#' @aliases hzID,SoilProfileCollection-method
#' @docType methods
#' @rdname hzID
#'
if (!isGeneric("hzID"))
  setGeneric("hzID", function(object)
    standardGeneric("hzID"))

setMethod("hzID", signature(object = "SoilProfileCollection"),
          function(object) {
            return(object@horizons[[hzidname(object)]])
          })

#' Get horizon designation column name
#'
#' @name hzdesgnname
#'
#' @description Get column name containing horizon designation name
#'
#' @param object a SoilProfileCollection
#' @aliases hzdesgnname,SoilProfileCollection-method
#' @docType methods
#' @rdname hzdesgnname
#'
if (!isGeneric("hzdesgnname"))
  setGeneric("hzdesgnname", function(object, ...)
    standardGeneric("hzdesgnname"))

## get column containing horizon designations (there is a setter of same name)
setMethod("hzdesgnname", signature(object = "SoilProfileCollection"),
          function(object) {
            res <- metadata(object)$aqp_hzdesgn
            if (length(res) == 0)
              res <- ""
            return(res)
          })

#' Get horizon designation column name
#'
#' @name hzDesgn
#'
#' @description Get horizon designation names
#'
#' @param object a SoilProfileCollection
#' @aliases hzDesgn,SoilProfileCollection-method
#' @docType methods
#' @rdname hzDesgn
#'
if (!isGeneric("hzDesgn"))
  setGeneric("hzDesgn", function(object, ...)
    standardGeneric("hzDesgn"))

setMethod("hzDesgn", signature(object = "SoilProfileCollection"),
          function(object) {

            h <- object@horizons
            hzd <- hzdesgnname(object)

            if (length(hzd)) {

              if (hzd %in% horizonNames(object)) {
                res <- h[[hzd]]
                return(res)
              }

            } else {

              stop("horizon designation name (",
                   hzd,
                   ") not in horizonNames().",
                   call. = FALSE)
            }

          })

#' Get horizon texture class column name
#'
#' @name hztexclname
#'
#' @description Get column name containing horizon designation name
#'
#' @param object a SoilProfileCollection
#' @aliases hztexclname,SoilProfileCollection-method
#' @docType methods
#' @rdname hztexclname
#'
if (!isGeneric("hztexclname"))
  setGeneric("hztexclname", function(object)
    standardGeneric("hztexclname"))

## get column containing horizon designations (there is a setter of same name)
setMethod("hztexclname", signature(object = "SoilProfileCollection"),
          function(object) {
              res <- metadata(object)$aqp_hztexcl
              if (length(res) == 0)
                res <- ""
              return(res)
            })

#' Get/set unique (sorted) profile IDs
#'
#' @name profile_id
#'
#' @description Get or set a vector of profile IDs
#'
#' @param object a SoilProfileCollection
#'
#' @aliases profile_id,SoilProfileCollection-method
#' @docType methods
#' @rdname profile_id
#'
## distinct profile IDs
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object)
    standardGeneric("profile_id"))

## relies on ordering of profile IDs in horizons matching site

setMethod("profile_id", signature(object = "SoilProfileCollection"),
          function(object)

            # ideally, we could rely on site(object)[[idname(object)]]

            unique(as.character(object@horizons[[idname(object)]])))

#' Get horizon depth column names
#'
#' @name horizonDepths
#'
#' @description Get column names containing horizon depths
#'
#' @param object a SoilProfileCollection
#' @aliases horizonDepths,SoilProfileCollection-method
#' @docType methods
#' @rdname horizonDepths
#'
if (!isGeneric("horizonDepths"))
  setGeneric("horizonDepths", function(object)
    standardGeneric("horizonDepths"))

setMethod("horizonDepths", signature(object = "SoilProfileCollection"),
          function(object)
            return(object@depthcols))


#' Get coordinates from spatial slot
#'
#' @name coordinates
#'
#' @description Get coordinates from spatial slot, if present.
#'
#' @param obj a SoilProfileCollection
#' @aliases coordinates,SoilProfileCollection-method
#' @docType methods
#' @rdname coordinates
#'
setMethod("coordinates", signature(obj = "SoilProfileCollection"),
          function(obj) {
            return(coordinates(obj@sp))
          })


## site data
if (!isGeneric("site"))
  setGeneric("site", function(object, ...)
    standardGeneric("site"))

#' Retrieve site data from SoilProfileCollection
#'
#' @name site
#'
#' @description Get site data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#' @aliases site,SoilProfileCollection-method
#' @docType methods
#' @rdname site
#'
setMethod("site", signature(object = "SoilProfileCollection"),
          function(object) {
            return(.as.data.frame.aqp(object@site, aqp_df_class(object)))
          })

#' Retrieve diagnostic data from SoilProfileCollection
#'
#' @name diagnostic_hz
#'
#' @description Get diatnostic data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#'
#' @aliases diagnostic_hz,SoilProfileCollection-method
#' @docType methods
#' @rdname diagnostic_hz
#'
## diagnostic horizons: stored as a data.frame
if (!isGeneric("diagnostic_hz"))
  setGeneric("diagnostic_hz", function(object, ...)
    standardGeneric("diagnostic_hz"))

setMethod(f = 'diagnostic_hz', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@diagnostic, aqp_df_class(object)))
          })

#' Retrieve restriction data from SoilProfileCollection
#'
#' @name restrictions
#'
#' @description Get restriction data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#' @aliases restrictions,SoilProfileCollection-method
#' @docType methods
#' @rdname restrictions
#'
## restrictions: stored as data.frame
if (!isGeneric("restrictions"))
  setGeneric("restrictions", function(object, ...)
    standardGeneric("restrictions"))

setMethod(f = 'restrictions', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@restrictions, aqp_df_class(object)))
          })

## horizon data
# returns a data.frame with horizons data
#' Retrieve horizon data from SoilProfileCollection
#'
#' @name horizons
#'
#' @description Get horizon data from SoilProfileCollection. Result is returned in the same \code{data.frame} class used to initially construct the SoilProfileCollection.
#'
#' @param object a SoilProfileCollection
#' @aliases horizons,SoilProfileCollection-method
#' @docType methods
#' @rdname horizons
#'
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod(f = 'horizons', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(.as.data.frame.aqp(object@horizons, metadata(object)$aqp_df_class))
          })

## metadata
# returns a data.frame
if (!isGeneric("metadata"))
  setGeneric("metadata", function(object, ...)
    standardGeneric("metadata"))

#' Retrieve metadata from SoilProfileCollection
#'
#' @name metadata
#'
#' @description Get metadata from SoilProfileCollection. Result is a list. Two entries (aqp_df_class, depth_units) should not be edited in the metadata list directly. There are methods that facilitate changing them -- and propagating their changes throughout the collection. Otherwise, metadata list is a free-form slot used to store arbitrary information about the data, how it was collected, citations, etc.
#'
#' @param object a SoilProfileCollection
#' @aliases metadata,SoilProfileCollection-method
#' @docType methods
#' @rdname metadata
#'
setMethod(f = 'metadata', signature(object = 'SoilProfileCollection'),
          function(object) {
            return(object@metadata)
          })

#' Get aqp_df_class entry from metadata or return a safe value.
#'
#' @name aqp_df_class
#'
#' @description This is an accessor method for the \code{aqp_df_class} entry in the metadata slot. This entry is used internally by methods that interact with \code{data.frame} objects and slots to ensure that the same class used to promote to the SoilProfileCollection initially is used throughout the process.
#'
#' @param object a SoilProfileCollection
#' @aliases aqp_df_class,SoilProfileCollection-method
#' @docType methods
#' @rdname aqp_df_class
#'
if (!isGeneric("aqp_df_class"))
  setGeneric("aqp_df_class", function(object, ...)
    standardGeneric("aqp_df_class"))

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

#' Get depth units from metadata
#'
#' @name depth_units
#'
#' @description Get units of depth measurement from metadata. Default value is centimeters.
#'
#' @param object a SoilProfileCollection
#' @aliases depth_units,SoilProfileCollection-method
#' @docType methods
#' @rdname depth_units
#'
if (!isGeneric("depth_units"))
  setGeneric("depth_units", function(object, ...)
    standardGeneric("depth_units"))

setMethod(f = 'depth_units', signature(object = 'SoilProfileCollection'),
          function(object) {
            u <- as.character(metadata(object)[['depth_units']])
            # give a warning if not defined
            if (length(u) == 0 | u == '')
              message('depth_units have not been defined')
            return(u)
          })


#' Get names of columns in site table
#'
#' @name siteNames
#'
#' @description Get names of columns in site table.
#'
#' @param object a SoilProfileCollection
#' @aliases siteNames,SoilProfileCollection-method
#' @docType methods
#' @rdname siteNames
#'
if (!isGeneric("siteNames"))
  setGeneric("siteNames", function(object, ...)
    standardGeneric("siteNames"))

setMethod("siteNames", signature(object = "SoilProfileCollection"),
          function(object) {
            res <- names(object@site)
            return(res)
          })

#' Get names of columns in horizon table
#'
#' @name horizonNames
#'
#' @description Get names of columns in horizon table.
#'
#' @param object a SoilProfileCollection
#' @aliases horizonNames,SoilProfileCollection-method
#' @docType methods
#' @rdname horizonNames
#'
if (!isGeneric("horizonNames"))
  setGeneric("horizonNames", function(object, ...)
    standardGeneric("horizonNames"))

setMethod("horizonNames", signature(object = "SoilProfileCollection"),
          function(object) {
            res <- names(object@horizons)
            return(res)
          })


##
## initialize metadata: object modification in-place
##
#' Set metadata for a SoilProfileCollection
#'
#' @name metadata<-
#' @param object A SoilProfileCollection
#' @param value A named list (see examples)
#'
#' @aliases metadata<-,SoilProfileCollection-method
#' @rdname metadata
#' @usage metadata(object) <- value
#' @examples
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
if (!isGeneric('metadata<-'))
  setGeneric('metadata<-', function(object, value)
    standardGeneric('metadata<-'))

setReplaceMethod("metadata", signature(object = "SoilProfileCollection"),
                 function(object, value) {

                   # metadata() is now stored in a list()
                   #
                   # quick sanity check
                   #if(nrow(value) > 1 | nrow(value) < 1)
                   #  stop("metadata should be a 1-row data frame", call.=FALSE)

                   # otherwise assign
                   object@metadata <- value

                   # done
                   return(object)
                 }
)

##
## initialize depth_units: object modification in-place, depth_units stored in @metadata
##
#' Set units of measurement for profile depth
#'
#' @name depth_units<-
#' @param object A SoilProfileCollection
#' @param value character, a value representing units. Default \code{'cm'}.
#'
#' @aliases depth_units<-,SoilProfileCollection-method
#' @rdname depth_units
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
#'
if (!isGeneric('depth_units<-'))
  setGeneric('depth_units<-', function(object, value) standardGeneric('depth_units<-'))

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
