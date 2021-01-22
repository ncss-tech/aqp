
.interpretHorizonColor <- function(h, color, default.color, col.palette, col.palette.bias, n.legend) {

  # this is optionally replaced with real data when using thematic colors
  color.legend.data <- NULL

  # toggle as needed for more room
  multi.row.legend <- FALSE
  # multi-row legend indices
  leg.row.indices <- NULL

  ## TODO: manually setting color=NULL will throw an error
  # think about how to best handle this
  # if(is.null(color)) {
  #
  # }

  # short-circuit: if all h[[color]] are NA the following logic will not reliably work
  # this is because sometimes all NA are interpreted as logical vectors
  if(all(is.na(h[[color]]))) {
    h[[".color"]] <- NA
  } else {

    # there is at least 1 non-NA color to work with

    # 1. numeric vector, rescale and apply color ramp
    if(is.numeric(h[[color]])) {

      # generate color ramp function
      cr <- colorRamp(col.palette, bias = col.palette.bias)

      if(!requireNamespace("scales", quietly = TRUE))
        stop("package `scales` is required", call.=FALSE)

      # note that this may contain NAs
      c.rgb <- cr(scales::rescale(h[[color]]))
      cc <- which(complete.cases(c.rgb))
      h$.color <- NA

      # convert non-NA values into colors
      h$.color[cc] <- rgb(c.rgb[cc, , drop = FALSE], maxColorValue=255)

      # generate range / colors for legend
      pretty.vals <- pretty(h[[color]], n = n.legend)

      # truncate to 3 signif vals and convert to character for correct interpretation of floating point values
      leg.pretty.vals <- as.character(signif(pretty.vals, 3))

      # special case: there are < 3 unique values -> convert to factor
      # previous calculations are ignored
      low.n.test.vals <- as.character(signif(h[[color]], digits = 3))
      if(length(unique(na.omit(low.n.test.vals))) < 3) {
        # replace with character representation with 3 significant digits
        h[[color]] <- low.n.test.vals
        message('less than 3 unique values, converting to factor')
      }

      # put into a list for later
      color.legend.data <- list(
        legend = leg.pretty.vals,
        col = rgb(cr(scales::rescale(pretty.vals)), maxColorValue=255),
        multi.row.legend = multi.row.legend,
        leg.row.indices = leg.row.indices
      )
    }

    # 2. vector of categorical data
    if(is.character(h[[color]]) | is.factor(h[[color]])) {

      # testing if ALL valid colors
      if( all(.isColorValid(na.omit(h[[color]])))) {
        # YES: interpret values directly as colors
        h$.color <- h[[color]]
      } else {
        # NO: this is or can be converted into a factor
        if(!is.factor(h[[color]]))
          h[[color]] <- factor(h[[color]])

        # get color mapping levels after dropping missing levels
        h[[color]] <- droplevels(h[[color]])
        color.levels <- levels(h[[color]])

        # make a color mapping function
        if(!requireNamespace("scales", quietly = TRUE))
          stop("package `scales` is required", call.=FALSE)

        color.mapper <- scales::col_factor(
          palette = colorRampPalette(col.palette, bias = col.palette.bias)(length(color.levels)),
          domain = color.levels,
          na.color = default.color,
          ordered = TRUE
        )

        # apply color mapping
        h$.color <- color.mapper(h[[color]])

        # generate colors and labels for legend
        pretty.vals <- color.levels

        # interpret n.legend as max(items) / row
        n.leg.classes <- length(pretty.vals)

        # create more room via multiple calls to legend
        if(n.legend < n.leg.classes) {

          # make indices to two rows of legends
          # safely accounts for even / odd n.leg.classes
          leg.row.indices <- .splitLegend(n.leg.classes)

          # set flag for later
          multi.row.legend <- TRUE
        }

        # pack into a list for later use
        color.legend.data <- list(
          legend = pretty.vals,
          col = color.mapper(pretty.vals),
          multi.row.legend = multi.row.legend,
          leg.row.indices = leg.row.indices
        )

      }
    }

  }


  # if the color column doesn't exist, fill with NA
  if(is.null(h[[color]]))
    h[[".color"]] <- NA

  # fill missing colors with a reasonable default
  h[['.color']] <- ifelse(is.na(h[['.color']]), default.color, h[['.color']])

  # assemble results
  res <- list(
    colors = h[['.color']],
    color.legend.data = color.legend.data
    )

  return(res)
}





# split legend into two rows, and create indices
# any more classes than that and things become impossible to read
# n: total number of classes
.splitLegend <- function(n) {

  #  make enough room for even division of odd numbers
  n.per.row <- ceiling(n / 2)

  # make indices for first row
  row.1.idx <- seq(from=1, to=n.per.row)
  row.2.idx <- seq(from=n.per.row + 1, to=n)

  res <- list(
    row.1=row.1.idx,
    row.2=row.2.idx
  )

  return(res)
}


# Test for valid colors in vector `x`:
#   1. named colors from colors()
#   2. RGB / RGBA encoding of colors
.isColorValid <- function(x) {
  # check for named colors
  test.1 <- x %in% colors()

  # check for valid RGB
  test.2 <- grepl('^#[a-f0-9]{6}', x, ignore.case = TRUE)

  # check for valid RGBA colors
  test.3 <- grepl('^#[a-f0-9]{8}', x, ignore.case = TRUE)

  # must pass at least 1 test
  res <- test.1 | test.2 | test.3
  return(res)
}



