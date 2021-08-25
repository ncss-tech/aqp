# if (!isGeneric("harmonize"))
  setGeneric("harmonize", function(x, x.names, keep.cols = NULL, grp.name = "hgroup")
    standardGeneric("harmonize"))

#' @title Harmonize a property by profile-level denormalization for convenient visualization or analysis of ranges
#'
#' @description It is sometimes convenient to be able to "denormalize" to a `SoilProfileCollection` with fewer attributes but more profiles. This is helpful wherever calculations are made on a profile basis and ranges or repeated measures are depicted with multiple attributes per soil horizon.
#'
#'  \code{harmonize} is most commonly used for creating "comparison" soil profile sketches with \code{plotSPC}--where the thematic attribute is derived from multiple data sources or summary statistics (such as quantiles of a property for Low-RV-High). However, the method more generally applies wherever one wants to alias between multiple columns containing "similar" data as input to an algorithm.
#'
#' Data are "harmonized" to a common attribute names specified by the names of list elements in \code{x.names}. Profiles are essentially duplicated. In order to satisfy uniqueness constraints of the `SoilProfileCollection`, the label from the sub-elements of \code{x.names} are used to disambiguate profiles. A new column in the site table is calculated to reflect these groupings and facilitate filtering. See examples below.
#'
#' @param x A `SoilProfileCollection`.
#' @param x.names a named list of character vectors specifying target names, profile ID suffixes and source attribute names for harmonization
#' @param keep.cols a character vector of column names to keep unaltered from the horizon data
#' @param grp.name a character vector with column name to store grouping variable in site table (default: "hgroup")
#'
#' @return A (redundant) `SoilProfileCollection`, with one profile for each set of harmonizations specified by \code{x.names}.
#'
#' @details If attributes reflecting the same or similar property within a soil layer have different names (e.g. \code{socQ05}, \code{socQ50}, \code{socQ95}) it is sometimes inconvenient to work with them as multiple attributes within the same profile. These similar attributes may need to be analyzed together, or in sequence by profile, displayed using the same name or using a common scale. It is also useful to be able to alias different data sources that have the same attributes with different names.
#'
#' Each list element in \code{x.names} specifies a single "harmonization," which is comprised of one or more mappings from new to old. Each named "sub-element" of \code{x.names} specifies the name and attribute to use for updating the profile ID and site table of  the duplicated profiles.
#'
#' @author Andrew G. Brown
#' @aliases harmonize
#' @examples
#'
#' ### single source "harmonization" of single-profile with range -> single attribute, multi-profile
#'
#' # make some test data
#' spc <- pbindlist(lapply(1:10, random_profile, SPC = TRUE))
#'
#' # assume that p1, p2 and p3 are the low RV and high quantiles for a hypothetical property "foo"
#' h1 <- harmonize(spc, x.names = list(foo = c(q05 = "p1", q50 = "p2", q95 = "p3")))
#'
#' # inspect result
#' plot(h1, color = "foo")
#'
#' # filter with calculated "harmonized group" to get just RV profiles
#' plot(subset(h1, hgroup == "q50"), color="foo")
#'
#' ### single source, two properties at once; with common labels: "method1" "method2"
#'
#' # assume that p1, p2 are measurements by two (!=) methods for a hypothetical property "foo"
#' #             p3, p4 are measurements by same two methods for a hypothetical property "bar"
#' h3 <- harmonize(spc, x.names = list(foo = c(method1 = "p1", method2 = "p2"),
#'                                     bar = c(method1 = "p3", method2 = "p4")))
#' plot(h3, color = "foo")
#' plot(h3, color = "bar")
#' head(horizons(h3))
#'
#' # a slight modification, "method 1" onlyused for "foo" and "method 3" for "bar"
#' h3 <- harmonize(spc, x.names = list(foo = c(method1 = "p1", method2 = "p2"),
#'                                     bar = c(method2 = "p3", method3 = "p4")))
#' plot(h3, color = "foo") # note the pattern of values missing for foo (*_method3)
#' plot(h3, color = "bar") #  likewise for bar (*_method1)
#'
#' #' the new labels need not match across harmonizations -- not sure how useful this is but it works
#' h3 <- harmonize(spc, x.names = list(foo = c(method1 = "p1", method2 = "p2"),
#'                                     bar = c(method3 = "p3", method4 = "p4")))
#' plot(h3, color = "foo") # note the pattern of values missing for foo (*_method 3 + 4)
#' plot(h3, color = "bar") #  likewise for bar (*_method 1 + 2)
#'
#' ### two-source harmonization
#'
#' # make test data
#' spc1 <- pbindlist(lapply(LETTERS[1:5], random_profile, SPC = TRUE))
#' spc2 <- pbindlist(lapply(letters[1:5], random_profile, SPC = TRUE))
#'
#' h4 <- pbindlist(list(harmonize(spc1, list(foo = c(transect1 = "p4"))),   # foo is p4 in dataset 1
#'                       harmonize(spc2, list(foo = c(transect2 = "p2")))))  # foo is p2 in dataset 2
#'
#' # same property with different name in two different datasets
#' plot(h4, color = "foo")
#'
#' ### many source harmonization
#'
#' # make test datasets (n=10); highly redundant IDs (1:3 repeated)
#' spcs <- lapply(1:10, function(x) pbindlist(lapply(1:3, random_profile, SPC = TRUE)))
#'
#' # randomly varying column name for demo (in each dataset, foo could could be p1 thru p5)
#' rcolname <- paste0("p", round(runif(10, 0.5, 5.5)))
#'
#' # iterate over data sources
#' bigspc <- pbindlist(lapply(1:length(spcs), function(i) {
#'
#'   # assume each data source has a unique name for the property "foo"
#'   xn <- rcolname[i]
#'
#'   # set names attribute to be equal to index i [creating unique profile IDs]
#'   #   i.e. 2_10 will be profile ID 2 from 10th dataset
#'   names(xn) <- i
#'
#'   # harmonize each data source, using unique column name and target name "foo"
#'   harmonize(spcs[[i]], x.names = list(foo = xn))
#' }))
#'
#' # inspect a subset
#' plot(bigspc[1:30,], color = "foo")
#'
setMethod("harmonize", signature(x = "SoilProfileCollection"),
          function(x, x.names, keep.cols = NULL, grp.name = "hgroup") {

  if (is.null(names(x.names)) | is.null(x.names) | !length(x.names) | !is.list(x.names))
    stop("argument `x.names` must be a named list with each element specifying a harmonization", call. = FALSE)

  if (missing(keep.cols) | is.null(keep.cols))
    keep.cols <- character(0)

  if (!is.character(grp.name) | (!length(keep.cols) > 0 & any(!keep.cols %in% horizonNames(x))))
    stop("argument `keep.cols` must be a character vector of column names to keep [unaltered] from horizon data", call. = FALSE)

  if (length(grp.name) != 1 | !is.character(grp.name))
    stop("argument `grp.name` must specify a single column name for output groups to be stored in site table", call. = FALSE)

  hname <- names(x.names)

  # required horizon-level attributes
  idn <- idname(x)
  dep <- horizonDepths(x)

  # optional horizon.level attributes
  hzd <- hzdesgnname(x)
  hzt <- hztexclname(x)
  hzcol <- c(idn, dep, hzd, hzt)
  hzcol <- hzcol[hzcol != ""]

  x.sets <- unique(unlist(lapply(x.names, names)))
  h.sets <- lapply(lapply(x.names, names), function(hn) x.sets %in% hn)

  # nested loop -- iterating over all profiles as many unique levels we have in elements of x.names
   pbindlist(profileApply(x, function(p) {
      pbindlist(lapply(1:length(x.sets), function(i) {

          # get horizon data for single profile
          p.out <- p
          hz.data <- horizons(p.out)

          xn <- unlist(lapply(x.names, function(xnn) xnn[x.sets[i] == names(xnn)]))

          # subset
          new.hz.data <- .data.frame.j(hz.data, c(hzcol, xn, keep.cols, hzidname(x)), aqp_df_class(x))

          # rename to "harmonized" column name
          hname.sub <- unlist(lapply(h.sets, function(x) x[i]))
          colnames(new.hz.data) <- c(hzcol, hname[hname.sub], keep.cols, hzidname(x))

          # replace horizon data with subset of required + harmonized
          replaceHorizons(p.out) <- new.hz.data

          # input x.names is a named vector, use those names as suffix on profile ID
          profile_id(p.out) <- paste(profile_id(p), x.sets[i], sep = "_")

          # add to site table also (useful for filtering downstream)
          p.out[[grp.name]] <- x.sets[i]

          # return modified profile
          return(p.out)
        }))
  }))
})
