
# 2022-03-29
interpolateChroma <- function(m.i) {
  
  # interpolation over S(y ~ C) and S(x ~ C)
  s.x <- splinefun(m.i$C, m.i$x)
  s.y <- splinefun(m.i$C, m.i$y)
  
  # make predictions along range of min(C) -> max(C)
  # but only where we are missing data
  C.original <- m.i$C
  C.full <- seq(from = min(m.i$C), to = max(m.i$C), by = 1)
  C.new <- setdiff(C.full, C.original)
  
  # eval spline functions along missing points
  p.x <- s.x(C.new)
  p.y <- s.y(C.new)
  
  # combine interpolated values into data.frame
  # H, V, Y are constant
  m.new <- data.frame(
    H = m.i$H[1],
    V = m.i$V[1],
    C = C.new,
    x = p.x,
    y = p.y,
    Y= m.i$Y[1]
  )
  
  # stack and re-order along C values
  m.combined <- rbind(m.i, m.new)
  m.combined <- m.combined[order(m.combined$C), ]
  
  # remove rownames
  row.names(m.combined) <- NULL
  
  return(m.combined)
}


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
# # compute midpoints between a sequence of points:
# mdpts <- function(x) 
# 	{
# 	m <- ( x[1:length(x)-1] + x[2:length(x)] ) / 2
# 	m
# 	}
# 



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
	M_adapt_C_to_D65 <- matrix(c(0.990448, -0.012371, -0.003564, -0.007168, 1.015594, 0.006770, -0.011615, -0.002928, 0.918157), ncol=3, byrow=TRUE)
	
	
	# 
	# to perform the chromatic adaption: convert from C -> D65 using Bradford method
	# 
	mun_XYZ_D65 <- mun_XYZ_C %*% M_adapt_C_to_D65
	
	
	return(mun_XYZ_D65)
	
	}


## 
## updated August 2009
## 
XYZ2rgb <- function(mun_XYZ_D65)
	{
	
	
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
	M_XYZ_to_sRGB_D65 <- matrix(c(3.24071, -0.969258, 0.0556352, -1.53726, 1.87599, -0.203996, -0.498571, 0.0415557, 1.05707), ncol=3, byrow=TRUE)
	
	
	
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
	
	
	return(data.frame(R=R_clip, G=G_clip, B=B_clip))
	}
	
	
