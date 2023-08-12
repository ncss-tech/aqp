
# generate depth axis labels
# m: max depth
# i: interval
.depthAxisSeq <- function(m, i = NULL) {
  
  # reasonable heuristics if an interval is not specified
  if(is.null(i)) {
    if(m > 800) {
      i <- 100
    } else if(m <= 800 & m > 200) {
      i <- 50
    } else if(m <= 200 & m > 100) {
      i <- 25
    } else if(m <= 100 & m > 50) {
      i <- 10
    } else if(m <= 50) {
      i <- 5
    }
  }
  
  # convenient rounding-to function
  # e.g. round 73, to nearest 10s place
  # thanks SO: https://stackoverflow.com/a/32508105
  .roundTo <- function(x, y) {
    if((y - x %% y) <= x %% y) { x + (y - x %% y)}
    else { x - (x %% y)}
  }
  
  # clamp axis max via customized rounding
  .max <- .roundTo(m, i)
  
  # axis sequence
  .res <- seq(0, .max, by = i)
  return(.res)
}



#' @title Draw a depth axis along side soil profile sketches
#'
#' @param style character, depth axis style
#' @param .at numeric vector, depth axis annotation coordinates
#' @param .labels character vector, tick mark labels
#' @param .line numeric, line offset for horizontal placement of axis
#' @param .cex numeric, scaling factor for axis
#' @param .n integer, horizontal space allocated for profile sketches (used for placement of 'tape' style depth axis)
#' 
#' @noRd
#' @return nothing, low-level plotting function
.drawDepthAxis <- function(style = c('compact', 'traditional', 'tape'), .at, .labels, .line, .cex, .n) {
  
  # launder / check style
  style <- tolower(style)
  style <- match.arg(style)
  
  switch(style,
         'compact' = {
           # minimal axis
           axis(
             side = 4, 
             col = NA, 
             col.axis = par('fg'), 
             col.ticks = par('fg'), 
             las = 1, 
             lend = 3, 
             mgp = c(3, 0.25, 0),
             font = 1, lwd.ticks = 1, tck = 0.02,
             line = .line, 
             at = .at, 
             labels = .labels, 
             cex.axis = .cex
           ) 
         },
         
         'traditional' = {
           # traditional axis
           axis(
             side = 4, 
             col = par('fg'), 
             col.axis = par('fg'), 
             col.ticks = par('fg'), 
             las = 1, 
             lend = 3, 
             line = .line, 
             at = .at, 
             labels = .labels, 
             cex.axis = .cex
           )
         },
         
         'tape' = {
           # something like the fabric, graduated tapes
           # used by NCSS and elsewhere
           # alternating colors
           .cols <- c('white', grey(0.3))
           
           # requires alternative calculation of position
           # n + line converted to user coordinates
           
           # convert line notation to user coordinates
           .dx <- grconvertX(.line, from = 'line', to = 'user')
           
           # shift using user coordinates of figure
           # this doesn't scale beyond 6 profiles
           # .x <- par('usr')[2] - .dx
           
           # shift relative to number of profiles or allocated space
           .x <- .n + .dx
           
           # width of tape based on widest depth annotation
           .w <- strwidth(as.character(max(.at)), units = 'user', cex = .cex) / 1.5
           .n <- length(.at)
           
           # alternating colors of tape
           rect(
             xleft = .x - .w,
             xright = .x + .w, 
             ybottom = .at[-1], 
             ytop = .at[-.n], 
             col = .cols, 
             border = NA
           )
           
           # outline
           rect(
             xleft = .x - .w, 
             xright = .x + .w, 
             ybottom = .at[.n], 
             ytop = .at[1],
             col = NA,
             border = par('fg')
           )
           
           # consider computing an exact coordinate, vs pos / offset
           # use at - (text height)
           # .labY <- .at[-1] - (diff(.at) / 2)
           .labY <- .at
           # seelct labels
           text(
             x = .x, 
             y = .labY, 
             labels = .at, 
             cex = .cex, 
             col = invertLabelColor(.cols), 
             offset = 0.25,
             pos = 1
           )

         }
  )
  
  # done
  
}


# draft replacement for scales::col_factor (without looking at it)
.aqp_color_map <- function(palette, domain, na.color, ordered) {
  .FUN <- function(x) {
    if (is.numeric(x)) {
      cuts <- c(gsub("^\\[(.*),.*", "\\1", domain[1]),
                gsub(".*,(.*)\\]$", "\\1", domain))
      x <- cut(x, breaks = cuts)
    } else {
      x <- factor(x, levels = domain, ordered = ordered)
    }
    idx <- as.integer(x)
    res <- palette[idx]
    res[is.na(res)] <- na.color
    res
  }
}

## generalize and make into an exported function

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
  if (all(is.na(h[[color]]))) {
    h[[".color"]] <- rep(NA_character_, nrow(h))
  } else {
    
    # there is at least 1 non-NA color to work with
    
    # 1. numeric vector, re-scale and apply color ramp
    if(is.numeric(h[[color]])) {
      
      # generate color ramp function
      cr <- grDevices::colorRamp(col.palette, bias = col.palette.bias)
      
      # re-scale to [0,1]
      # may contain NAs
      c.rgb <- cr(.rescaleRange(h[[color]], x0 = 0, x1 = 1))
      cc <- which(complete.cases(c.rgb))
      h$.color <- NA
      
      # convert non-NA values into colors
      h$.color[cc] <- rgb(c.rgb[cc, , drop = FALSE], maxColorValue = 255)
      
      # generate range / colors for legend
      pretty.vals <- pretty(h[[color]], n = n.legend)
      
      ## TODO: think about a smarter way to do this
      ##       -> sometimes the colors used in the legend are misleading
      
      # constrain legend to min/max
      # pretty.vals[1] <- min(h[[color]], na.rm = TRUE)
      # pretty.vals[length(pretty.vals)] <- max(h[[color]], na.rm = TRUE)
      
      # truncate to 3 significant digits and convert to character for correct interpretation of floating point values
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
        col = rgb(cr(.rescaleRange(pretty.vals, x0 = 0, x1 = 1)), maxColorValue=255),
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
        
        crp <- grDevices::colorRampPalette(col.palette, bias = col.palette.bias)
        
        # make a color mapping function
        if (requireNamespace("scales", quietly = TRUE)) {
          ## TODO: replace with native function
          FUN <- scales::col_factor
        } else {
          FUN <- .aqp_color_map
        }
        
        color.mapper <- FUN(
          palette = crp(length(color.levels)),
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


# x1: left-side x coordinate
# x2: right-side x coordinate
# y:  common y coordinate
# n: number of vertices
# o: vertical offsets (must be pre-scaled by plotSPC's scaling.factor argument)
# f: jittering factor
.raggedLines <- function(x1, x2, y, n = 16, o = c(-0.25, 0.75), f = 1) {
  
  # initial vertices, includes left and right points
  .x <- seq(from = x1, to = x2, length.out = n)
  .y <- rep(y, times = n)
  
  # add offsets + jitter to all but outermost vertices
  .idx <- 2:(n-1)
  .y[.idx] <- jitter(.y[.idx] + o, factor = f)
  
  # TODO: convert to step function, would look better on screen
  
  # final coordinates
  return(cbind(.x, .y))
}



