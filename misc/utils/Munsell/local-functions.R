


## TODO: clamp to original range of Munsell chroma

# interpolation of odd chroma
interpolateOddChromaSpectra <- function(i) {
  
  # 0-row input
  if(nrow(i) < 1) {
    return(NULL)
  }
    
  # chroma stats
  u.chroma <- unique(i$chroma)
  r.chroma <- range(u.chroma)
  n.chroma <- length(u.chroma)
  
  # reflectance stats
  r.reflectance <- range(i$reflectance)
  
  # sequence of candidate chroma
  # odd numbers, including 1 if missing
  s <- seq(from = r.chroma[1], to = r.chroma[2], by = 1)
  s <- c(1, s)
  s.chroma <- setdiff(s, u.chroma)
  
  # short circuit: single chroma, interpolation impossible
  if(n.chroma < 2)
    return(NULL)
  
  # short circuit: 0 candidates for interpolation
  if(length(s.chroma) < 1)
    return(NULL)
  
  
  # setup interpolation function: natural splines
  # fit is exact at training points
  sf <- splinefun(i$chroma, i$reflectance, method = 'natural')
  
  # check: fit should be exact at points
  if(sum(sf(i$chroma) - i$reflectance) > 0.001){
    message('spline not fitting at training data!')
  }
  
  # interpolate candidate chroma
  s.reflectance <- sf(s.chroma)
  
  # re-assemble into original format
  res <- data.frame(
    munsell = sprintf("%s %s/%s", i$hue[1], i$value[1], s.chroma),
    hue = i$hue[1],
    value = i$value[1],
    chroma = s.chroma,
    wavelength = i$wavelength[1],
    reflectance = s.reflectance,
    stringsAsFactors = FALSE
  )
  
  return(res)
}


interpolateValueSpectra <- function(i) {
  
  # new Munsell values
  # there are no spectra > value 9 (except neutral chips)
  v.target <- c(2.5, 8.5, 9)
  
  # 0 or 1 row input: no interpolation possible
  if(nrow(i) < 2) {
    return(NULL)
  }
  
  # always ignore neutral spectra
  if(any(i$hue == 'N')) {
    return(NULL)
  }
  
  
  # there are a few spectra associated with 8.5 values
  # if present, ignore 
  v.target <- setdiff(v.target, i$value)
  
  # setup interpolation function: natural splines
  # fit is exact at training points
  a.fun <- splinefun(i$value, i$reflectance, method = 'natural')
  
  # re-assemble into original format
  res <- data.frame(
    munsell = sprintf("%s %s/%s", i$hue[1], v.target, i$chroma[1]),
    hue = i$hue[1],
    value = v.target,
    chroma = i$chroma[1],
    wavelength = i$wavelength[1],
    reflectance = a.fun(v.target),
    stringsAsFactors = FALSE
  )
  
  return(res)
}





# 2022-03-29
# interpolate odd chroma from Munsell renotation data
# m.i: subset renotation data.frame, for a single hue/value
interpolateChroma <- function(m.i) {
  
  # spline interpolation over S(y ~ C) and S(x ~ C)
  # fit is exact at training points
  s.x <- splinefun(m.i$C, m.i$x, method = 'natural')
  s.y <- splinefun(m.i$C, m.i$y, method = 'natural')
  
  # make predictions along range of 1 -> max(C)
  # but only where we are missing data
  .original <- m.i$C
  .full <- seq(from = 1, to = max(m.i$C), by = 1)
  .new <- setdiff(.full, .original)
  
  # combine interpolated values into data.frame
  # H, V, Y are constant
  m.new <- data.frame(
    H = m.i$H[1],
    V = m.i$V[1],
    C = .new,
    x = s.x(.new),
    y = s.y(.new),
    Y = m.i$Y[1]
  )
  
  # stack and re-order along C values
  m.combined <- rbind(m.i, m.new)
  m.combined <- m.combined[order(m.combined$C), ]
  
  # remove rownames
  row.names(m.combined) <- NULL
  
  return(m.combined)
}


# 2024-09-26
# re-write of interpolateValue() -> now safely interpolates all 0.5 values
# m.i: data.frame of Munsell renotation data, for a single hue and chroma
interpolateValue2 <- function(m.i) {
  
  # can only proceed with >=2 rows
  # some combinations of hue, value, chroma have 1 row.
  # there will be other combinations created by split() with 0 rows
  if(nrow(m.i) < 2) {
    return(NULL)
  }
  
  # spline interpolation ~ munsell value
  # fit is exact at training points
  # x ~ V
  s.1 <- splinefun(m.i$V, m.i$x, method = 'natural')
  # y ~ V
  s.2 <- splinefun(m.i$V, m.i$y, method = 'natural')
  # Y ~ V
  s.3 <- splinefun(m.i$V, m.i$Y, method = 'natural')
  
  # new Munsell values for which interpolated xyY coordinates are required
  # limited to the range of available value at this hue/chroma combination
  new.V <- seq(from = min(m.i$V) + 0.5, to = max(m.i$V) - 0.5, by = 1)
  
  # TODO: coordinate with interpolation of spectra
  
  # combine interpolated values into data.frame
  # H, C are constant
  m.new <- data.frame(
    H = m.i$H[1],
    V = new.V,
    C = m.i$C[1],
    p1 = s.1(new.V),
    p2 = s.2(new.V),
    p3 = s.3(new.V)
  )
  
  names(m.new) <- c('H', 'V', 'C', 'x', 'y', 'Y')
  
  # only return new rows
  return(m.new)
}


## NOTE: this can only interpolate between two integer values
# 2022-03-29
# for now only interpolating 2.5 value
# usually interpolating xyY, 
# but important to interpolate sRGB for neutral hues
interpolateValue <- function(m.i, new.V = 2.5, vars = c('x', 'y', 'Y')) {
  
  # linear interpolation over value
  # x ~ V
  s.1 <- approxfun(m.i$V, m.i[[vars[1]]])
  # y ~ V
  s.2 <- approxfun(m.i$V, m.i[[vars[2]]])
  # Y ~ V
  s.3 <- approxfun(m.i$V, m.i[[vars[3]]])
  
  # combine interpolated values into data.frame
  # H, C are constant
  m.new <- data.frame(
    H = m.i$H[1],
    V = new.V,
    C = m.i$C[1],
    p1 = s.1(new.V),
    p2 = s.2(new.V),
    p3 = s.3(new.V)
  )
    
  names(m.new) <- c('H', 'V', 'C', vars)
  
  # only return new rows
  return(m.new)
}






# 
# pass this a data frame with x, y, and Y columns
# 
# this function expects:
# 
# x and y are approx (0,1)
# Y is approx (0,1)
# 
## updated Jan 2008
## manual conversion	
# 
# convert xyY to XYZ with:
# http://www.brucelindbloom.com/Eqn_xyY_to_XYZ.html
# 
xyY2XYZ <- function(xyY.data) {
	
	# x and y are approx (0,1)
	# Y is approx (0,1)
	# conversion to XYZ color space	
	X <- (xyY.data$x * xyY.data$Y ) / xyY.data$y
	Y <- xyY.data$Y
	Z <- ( (1- xyY.data$x - xyY.data$y) * xyY.data$Y )  / xyY.data$y
	
	# combine to form matrix for simple manipulation
	mun_XYZ_C <- matrix(c(X,Y,Z), ncol=3)
	
	# test for y == 0
	# X,Y,Z should then be set to 0
	mun_XYZ_C[which(xyY.data$y==0),] <- c(0,0,0)
	
	# 
	# functions in the colorspace package, and sRGB profiles assume a D65 illuminant
	# 
	# therefore we need to perform a chromatic adaption transform
	# 
	# http://www.brucelindbloom.com/Eqn_ChromAdapt.html
	# 
		
	## this has been revised as of Jan 2008 
	## new version:
	M_adapt_C_to_D65 <- matrix(
	  c(0.990448, -0.012371, -0.003564, -0.007168, 1.015594, 0.006770, -0.011615, -0.002928, 0.918157), 
	  ncol = 3, 
	  byrow = TRUE
	)
	
	
	# 
	# to perform the chromatic adaption: convert from C -> D65 using Bradford method
	# 
	mun_XYZ_D65 <- mun_XYZ_C %*% M_adapt_C_to_D65
	
	
	return(mun_XYZ_D65)
	
	}


## 
## updated August 2009
## 
XYZ2rgb <- function(mun_XYZ_D65) {
	
	
	# 
	# how different are the two? not that.
	# 
	# summary((mun_XYZ_D65 - mun_XYZ_C)  )
	
	
	
	# 
	# convert to (s)RGB manually: 
	# 
	# this assumes that XYZ is scaled to (0,1)
	# 
	# 
	# first get the reference primaries: 
	# http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
	# 
	# sRGB profile:
	M_XYZ_to_sRGB_D65 <- matrix(
	  c(3.24071, -0.969258, 0.0556352, -1.53726, 1.87599, -0.203996, -0.498571, 0.0415557, 1.05707), 
	  ncol = 3, 
	  byrow = TRUE
	)
	
	
	
	# apply the conversion matrix
	mun_sRGB_D65 <- mun_XYZ_D65 %*% M_XYZ_to_sRGB_D65
	
	
	# 
	# now convert r,g,b to RGB (sRGB, gamma = 2.4)
	# 
	# define the transformation functions: 
	# http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_RGB.html
	#
	fun1 <- function(col_comp) { 1.055 * ( col_comp ^ ( 1 / 2.4 ) ) - 0.055 } 
	fun2 <- function(col_comp) { 12.92 * col_comp } 
	
	# 
	# the specific function is contingent on the absolute value of r,g,b components
	# 
	R <- ifelse(mun_sRGB_D65[,1] >= 0.0031308, fun1(mun_sRGB_D65[,1]), fun2(mun_sRGB_D65[,1]))  
	G <- ifelse(mun_sRGB_D65[,2] >= 0.0031308, fun1(mun_sRGB_D65[,2]), fun2(mun_sRGB_D65[,2]))  
	B <- ifelse(mun_sRGB_D65[,3] >= 0.0031308, fun1(mun_sRGB_D65[,3]), fun2(mun_sRGB_D65[,3]))  
	
	
	#clip values to range {0,1}
	R_clip <- ifelse(R < 0, 0, R)  
	G_clip <- ifelse(G < 0, 0, G)  
	B_clip <- ifelse(B < 0, 0, B)  
	
	R_clip <- ifelse(R > 1, 1, R_clip)  
	G_clip <- ifelse(G > 1, 1, G_clip)  
	B_clip <- ifelse(B > 1, 1, B_clip) 
	
	
	return(data.frame(R = R_clip, G = G_clip, B = B_clip))
	}
	
	
