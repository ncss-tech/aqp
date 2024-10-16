
## TODO: this could use an overhaul:
##  * symbol size / resolution should be determined in context
##  * area approximation should work with a larger grid, after scaling to avoid fractional thickness constraints
##  * standardize symbology with rect() vs. points() and cex
##   -> init checkerboard, select squares, fill squares
##   -> number / size of squares is the only parameter

## doesn't work with fractional depths: https://github.com/ncss-tech/aqp/issues/8

# convert volume pct [0, 100] into DF of points along a res x res grid
# v should not be NA
.volume2df <- function(v, depth, res) {
  
  # test for >= 100%
  if(v >= 100) {
    warning(sprintf("%s is >= 100, likely a data error", v), call. = FALSE)
    # truncate at 100%
    v <- 100
  }
  
  # convert volume pct into fraction
  v <- v / 100
  
  # init matrix with NA
  cells <- (depth * res)
  m <- matrix(nrow = depth, ncol = res)
  # set to 1 for debugging output (full grid)
  m[] <- NA 
  
  # determine number of cells required to symbolize volume fraction
  # v must be >0 -> select a single cell when v is <1
  v.n <- pmax(round(v * cells), 1)
  
  # random sample of cells, mark with 1 in matrix
  v.cells <- sample(1:cells, size = v.n)
  m[v.cells] <- 1
  
  # convert matrix into data.frame
  d <- expand.grid(x = (1:res), y = (1:depth))
  d$val <- m[1:cells]
  # keep only those cells with val = 1
  d <- d[which(d$val == 1), ]
  
  # scrub extra columns and return
  d$val <- NULL
  return(d)
}


#' @title Symbolize Volume Fraction within a Soil Profile Collection Plot
#' 
#' @description Symbolize volume fraction on an existing soil profile collection plot.
#'
#' @param x a `SoilProfileCollection` object
#' @param colname character vector of length 1, naming the column containing volume fraction data (horizon-level attribute). Values should be within 0-100 percent.
#' @param res integer, resolution of the grid used to symbolize volume fraction
#' @param cex.min minimum symbol size
#' @param cex.max maximum symbol size
#' @param pch integer, plotting character code
#' @param col symbol color, either a single color or as many colors as there are horizons in `x`
#' 
#' @details This function can only be called after plotting a `SoilProfileCollection` object. Details associated with a call to `plotSPC()` are automatically accounted for within this function: e.g. `plot.order`, `width`, etc..
#' 
#' @author D.E. Beaudette
#' 
#' @seealso [plotSPC()]
#'
#' @export
#' 


## TODO: symbol size must be controlled by `res`
addVolumeFraction <- function(x, colname, res = 10, cex.min = 0.1, cex.max = 0.5, pch = 1, col = 'black') {
  
  # color should be either:
  # single color name / code
  # vector of colors with length == nrow(x)
  
  # simplest case, single color vector
  if(length(col) == 1) {
    col <- rep(col, times=nrow(x))
  } else {
    # check to make sure that vector of colors is the same length as number of horizons
    if(length(col) != nrow(x))
      stop('length of `col` should be either 1 or nrow(x)', call. = FALSE)
  }
  
  # ensure that `colname` is a horizon-level attribute
  if(! colname %in% horizonNames(x)) {
    stop(sprintf("%s is not a horizon-level attribute", colname), call. = FALSE)
  }
  
  # test for values < 0.5, could be a fraction [0,1] vs. percent [0,100]
  if(all( na.omit(horizons(x)[[colname]]) < 0.5) ) {
    message(sprintf("all %s values are < 0.5, likely a fraction vs. percent", colname))
  }
  
  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir = aqp.env)
  w <- lsp$width
  plot.order <- lsp$plot.order
  
  # y.offset is a vector of length(x)
  depth.offset <- lsp$y.offset
  sf <- lsp$scaling.factor
  x0 <- lsp$x0
  
  # horizontal shrinkage factor
  # hard-coded based on aesthetic tests
  w.offset <- w / 7
  
  # get top/bottom colnames
  hd <- horizonDepths(x)
  
  # iterate over profiles
  for(p.i in 1:length(x)) {
    
    # get the current pofile, in plotting order
    h <- horizons(x[plot.order[p.i], ])
    
    ## determine left/right extent of symbols
    # 2019-07-15: using relative position as indexed by current profile
    x.center <- x0[p.i]
    x.left <- x.center - (w - w.offset)
    x.right <- x.center + (w - w.offset)
    
    # iterate over horizons
    for(h.i in 1:nrow(h)) {
      this.hz <- h[h.i, ]
      hz.thick <- this.hz[[hd[2]]] - this.hz[[hd[1]]]
      
      ## hack until #8 is resolved: round the thickness down
      # https://github.com/ncss-tech/aqp/issues/8
      if( hz.thick %% 1 != 0) {
        # truncate at closest integer
        # minimum value of 1 to ensure something is plotted within very thin horizons
        hz.thick <- pmax(floor(hz.thick), 1)
        
        msg <- sprintf(
          "%s%s -> %s%s thick", 
          paste(h[h.i, hd], collapse = ' - '), 
          depth_units(x), 
          hz.thick, 
          depth_units(x)
        )
        message(sprintf('truncating fractional horizon thickness to integer: %s', msg))
        
      }
      
      # proceed only if the current profile/horizon:
      #  * value not NA
      #  * value > 0
      #  * hz.thick not NA
      #  * hz.thick > 0
      
      if((hz.thick > 0) & (!is.na(hz.thick)) & (this.hz[[colname]] > 0) & (! is.na(this.hz[[colname]]))) {
        
        # convert this horizon's data
        v <- .volume2df(v = this.hz[[colname]], depth = hz.thick, res = res)
        
        ## TODO: this can be better solved with dynamic 'res'
        # if (thickness * volume * res) is too small, there may be no samples returned
        # skip
        if(nrow(v) < 1) {
          # print('skipping...')
          next
        }
        
        # get the current color from vector of colors
        # typically the same color repeated, but could be as many colors as hz
        # may be NA if using a horizon column
        v$color <- col[h.i]
        
        # jitter and rescale x-coordinates
        v$x <- .rescaleRange(jitter(v$x), x0 = x.left, x1 = x.right)
        
        # determine horizon depths in current setting
        # depth_prime = (depth * scaling factor) + y.offset[i]
        y.top <- (this.hz[[hd[1]]] * sf) + depth.offset[p.i]
        y.bottom <- (this.hz[[hd[2]]] * sf) + depth.offset[p.i]
        
        # rescale y-coordinates
        v$y <- .rescaleRange(jitter(v$y), x0 = y.top, x1 = y.bottom)
        
        # generate random symbol size
        p.cex <- runif(nrow(v), min = cex.min, max = cex.max)
        
        # add jittered points
        # note that color comes from `v`
        points(v$x, v$y, cex = p.cex, col = v$color, pch = pch)
      }
      
    } # end iteration over horizons
  } # end iteration over profiles
  
}









