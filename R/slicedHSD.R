
#' .parseHSD_formula
#' @description internally used function to parse slicedHSD formula notation
#' @param fm to parse, 0:100 ~ variable | group
#' @return list with formula pieces
.parseGrouped_formula <- function(fm) {

  ## testing
  # fm <- 0:100 ~ estimated_oc | taxonname

  ## parse formula
  # string processing to get the LHS
  # expression parsing to get the RHS

  # extract components of the formula:
  fm.str <- paste(deparse(fm, 500), collapse = '')
  fm.elements <- str_split(fm.str, fixed("~"))[[1]]
  fm.elements <- lapply(str_split(fm.elements, "[+*]"), str_trim)

  # sanity check:
  if(length(fm.elements) != 2) {
    stop('error in `fm`, should be top:bottom ~ var | group', call. = FALSE)
  }

  # extract depth sequence
  depth.sequence <- fm.elements[[1]]

  # extract var | group
  var.group <- all.vars(update(fm, 0~.)) # right-hand side

  # sanity check
  if(length(var.group) != 2) {
    stop('error in `fm`, should be top:bottom ~ var | group', call. = FALSE)
  }

  # done
  return(list(fm = fm, depth.sequence = depth.sequence, var = var.group[1], group = var.group[2]))
}


#' .HSD
#' @description Safely compute Tukey's HSD within a given slice
#' @param z data.frame containing basic metadata, horizon top/bottom, variable of interest, and grouping variable
#' @param aov.fm formula suitable for aov(): variable ~ group
#' @param conf confidence level for TukeyHSD
.HSD <- function(z, aov.fm, conf = 0.95) {

  # sanity checks

  ## TODO: probably a better way to move names around

  # output template for error conditions
  # use first row of input

  # top depth is the second column
  # bottom depth is the third column
  d <- data.frame(
    top = z[1, 2],
    bottom = z[1, 3],
    diff = NA,
    lwr = NA,
    upr = NA,
    p.adj = NA
  )

  # propogate horizon depth names
  names(d)[1:2] <- names(z)[2:3]


  # fit model and catch errors
  mod <- try(
    aov(aov.fm, data = z),
    silent = TRUE
    )

  # sometimes there aren't enough data
  # return NA results
  if(inherits(mod, 'try-error')) {
    return(d)
  }

  # does this need error trapping?
  res <- TukeyHSD(mod, conf.level = conf)

  # results: only works with A/B style testing of two groups
  d$diff <- res[[1]][1]
  d$lwr <- res[[1]][2]
  d$upr <- res[[1]][3]
  d$p.adj <- res[[1]][4]

  return(d)
}



#' @title Tukey's HSD Over Slices
#' @description Apply Tukey's HSD over 1cm slices.
#'
#' @author D.E. Beaudette and Sharon Perrone
#'
#' @param object \code{SoilProfileCollection} object
#' @param fm a formula describing depth sequence, horizon attribute, and site (grouping) attribute.
#' For example 0:100 ~ estimated_oc | taxonname
#' @param conf confidence applied in \code{TukeyHSD}
#'
slicedHSD <- function(object, fm, conf = 0.95) {

  # parse specialized HSD formula
  fm.data <- .parseGrouped_formula(fm)

  # sanity checks: is this an SPC?
  if(! inherits(object, 'SoilProfileCollection')) {
    stop('`object` should be a SoilProfileCollection', call. = FALSE)
  }

  # sanity check: do the variables specified in fm exist?
  # note that there is only a single variable and single group
  if(
    ! fm.data$var %in% horizonNames(object) |
    ! fm.data$group %in% siteNames(object)
  ) {
    stop('`fm` must reference a single horizon-level attribute and a single site-level attribute', call. = FALSE)
  }


  # parse depth sequence
  depth.sequence <- eval(parse(text = fm.data$depth.sequence))

  # create slice formula
  slice.fm <- as.formula(sprintf("%s ~ %s", fm.data$depth.sequence, fm.data$var))

  # create slab formula
  slab.fm <- as.formula(sprintf("%s ~ %s", fm.data$group, fm.data$var))

  # create AOV formula
  aov.fm <- as.formula(sprintf("%s ~ %s", fm.data$var, fm.data$group))

  ## TODO: this would be a heck of a lot simpler with a slab() re-write
  # https://github.com/ncss-tech/aqp/issues/3

  ## for now, using slice + slab

  # slice for HSD iteration
  # don't enforce strict horizon depth checking
  # use results with caution
  s <- slice(object, fm = slice.fm, strict = FALSE)

  # aggregate by group over slices
  a <- slab(object, fm = slab.fm, slab.structure = depth.sequence)


  # HSD iteration uses only a single site-level attr defining groups and some horizon data
  # hmmm.. does it make sense to cut-down to just those variables that matter?
  ## TODO: this will need some special treatment for tibble / data.table
  hzd <- horizonDepths(s)
  vars <- c(idname(s), hzd, hzidname(s), fm.data$var)

  # convert to data.frame
  x <- horizons(s)[, vars, drop = FALSE]
  # move group from site -> hz
  x[[fm.data$group]] <- denormalize(s, fm.data$group)

  ## TODO: this needs to be more robust
  ## generate slice / segment labels
  # genSlabLabels(slab.structure = depth.sequence, max.d = max(depth.sequence), n.profiles = length(object))

  ## TODO: this will not work for larger slabs, will have to implement
  # split according to slice top depths
  xx <- split(x, x[[hzd[1]]])
  n.slices <- length(xx)

  # iterate over slice-index and perform Tukey's HSD
  # this likely violates the "multiple comparison" criteria... but I am not a statistician
  HSD <- lapply(1:n.slices, function(i) {
    .HSD(z = xx[[i]], aov.fm = aov.fm, conf = conf)
  })

  # list -> DF
  HSD <- do.call('rbind', HSD)

  # pack everything needed to plot into a list
  res <- list(
    HSD = HSD,
    agg = a,
    min.depth = min(depth.sequence),
    max.depth = max(depth.sequence),
    HSD.conf = conf
  )


  # done
  return(res)

}

