

## New 5/17/2010
# pass in data with values >= 1
# applied to data for a single Hue / Value
interpolate_munsell_chroma <- function(the.data)
	{
	
	# generate spline functions for x and y, based on C
	s.x <- splinefun(the.data$C, the.data$x)
	s.y <- splinefun(the.data$C, the.data$y)
	
	# make predictions along range of 1:max(C), 
	# but only where we are missing data
	range.original <- the.data$C
	range.new <- which(! 1:max(the.data$C) %in% range.original)
	
	# eval spline functions along missing points
	p.x <- s.x(range.new)
	p.y <- s.y(range.new)
	
	# combine interpolated values into dataframe
	d.new <- data.frame(
	H=rep(unique(the.data$H), times=length(range.new)),
	V=rep(unique(the.data$V), times=length(range.new)),
	C=range.new,
	x=p.x,
	y=p.y,
	Y=rep(unique(the.data$Y), times=length(range.new))
	)
	
	# re-order along C values
	d.combined <- rbind(the.data, d.new)
	return(d.combined[order(d.combined$C), ])
	}


## old version
# 
# # 
# # needs a dataframe with H,V,C and x,y,Y columns
# # 
# # 
# interpolate_munsell_chroma <- function(the.data)
# 	{
# 	
# 		
# 	# init a list to hold the results
# 	res <- list()
# 	
# 	# init a counter:
# 	i <- 1
# 	
# 	# iterate over every Hue
# 	for(hue in as.character(levels(the.data$H)) ) 
# 		{
# 		
# 		# iterate over every value within this hue
# 		for(val in unique(the.data$V[the.data$V >= 1]) )
# 			{
# 			# extract x,y,C coordinates for this Hue
# 			a <- subset(the.data, subset=H == hue & V == val)
# 			
# 			# since Y is constant pickup the first one in the sequence
# 			# we will use this later
# 			this.Y <- a$Y[1]
# 			
# 			# compute midpoints (odd chromas)
# 			# note that result is a matrix-like object
# 			# if there is only one row, as.data.frame() will give an odd result
# 			m <- apply(a[,c('x', 'y', 'C')], 2, mdpts)
# 			
# 			##
# 			## for some reason we cannot coerce results from apply into a DF when there is
# 			## only one row returned...
# 			##		
# 			if(nrow(a) > 2)
# 				{
# 				n_times <- nrow(m)
# 				
# 				m.1 <- as.data.frame(m)
# 				# since the hue, value, and Y coordinates are constant within this loop
# 				# combine and save to item [[i]] in the list:
# 				res[[i]] <- data.frame(H=rep(as.character(hue), n_times), V=rep(val, n_times), C=m.1$C, x=m.1$x, y=m.1$y, Y=rep(this.Y, n_times) )
# 				
# 				}
# 			else
# 				{
# 				n_times <-1
# 				
# 				# since the hue, value, and Y coordinates are constant within this loop
# 				# combine and save to item [[i]] in the list:
# 				res[[i]] <- data.frame(H=rep(as.character(hue), n_times), V=rep(val, n_times), C=m['C'], x=m['x'], y=m['y'], Y=rep(this.Y, n_times) )
# 				
# 				# debugging		
# 				# print(a)
# 				}
# 			
# 			
# 			# increment counter
# 			i <- i + 1
# 			}
# 		}
# 	
# 	
# 	# put this the.data back together again:
# 	# 
# 	# idea: rbind(res[[1]], res[[2]], ...)
# 	# 
# 	# note that number of cols in each list must be the same
# 	# which(sapply(res, ncol) != 6)
# 	# 
# 	res.combined <- do.call("rbind", res)
# 	
# 	# return the results
# 	res.combined
# 	
# 	}
# 




# compute midpoints between a sequence of points:
mdpts <- function(x) 
	{
	m <- ( x[1:length(x)-1] + x[2:length(x)] ) / 2
	m
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
xyY2XYZ <- function(xyY.data)
	{
	
	# x and y are approx (0,1)
	# Y is approx (0,1)
	#conversion to XYZ color space	
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
	
	
