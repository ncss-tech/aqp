# 
#' DEPRECATED Simulate Soil Profiles
#' 
#' @description Simulate a collection of soil profiles based on the horizonation of a single soil profile. Now deprecated: use [perturb()] for perturbations of horizon thicknesses or boundaries.
#' 
#' @param x a SoilProfileCollection object containing a single profile from which to draw simulated data
#' @param n the number of requested simulations
#' @param iterations sampling iterations used to determine each horizon thickness
#' @param hz.sd standard deviation used to simulate horizon thickness, can be a vector but must divide evenly into the number of horizons found in \code{x}
#' @param min.thick minimum horizon thickness allowed in simulation results
#'
#' @return A SoilProfileCollection object with \code{n} simulated profiles, each containing the same number of horizons and same data as \code{x}
#' 
#' @details This function generates a collection of simulated soil profiles based on the horizon thickness data associated with a single "template" profile. Simulation is based on sampling from a family of Gaussian distribution with means defined by the "template" profile and standard deviation defined by the user.
#' 
#' @export
#' 
#' @seealso \code{\link{random_profile}}  \code{\link{perturb}}
#' 
#' @author D. E. Beaudette
#' 
#' @examples
#' 
#' # please see documentation for perturb() for examples
#' #  the sim() function calls perturb() internally
#' 
sim <- function(x,
                n = 1,
                iterations = 25,
                hz.sd = 2,
                min.thick = 2) {	
  
  .Deprecated("perturb")
  
  horizons(x)$.perturb_internalHzSD <- hz.sd

  res <- perturb(x, 
                 n = n, 
                 thickness.attr = ".perturb_internalHzSD",
                 min.thickness = min.thick,
                 max.depth = NULL)
  
  res$.perturb_internalHzSD <- NULL
  return(res)
#   hd <- horizonDepths(x)
# 	h <- horizons(x)
# 	thick <- h[[hd[2]]] - h[[hd[1]]]
# 	
# 	# remove original depth columns
# 	h[[hd[1]]] <- NULL
# 	h[[hd[2]]] <- NULL
# 	
# 	# remove horizon ID so as not to create conflicts later on
# 	h[[hzidname(x)]] <- NULL
# 	
# 	# keep track of old id
# 	old.id.name <- idname(x)
# 	
# 	# sanity checks
# 	if(length(x) > 1)
# 		stop('this function can only simulate data from a SoilProfileCollection containing a single profile')
# 	
# 	if(length(thick) %% length(hz.sd) != 0)
# 		stop('the length of hz.sd must divide evenly into the number of horizons', call.=FALSE)
# 	
# 	# define function to wrap rnorm for use with outer
# 	rnorm.vect <- function(mean, sd, n) {
# 		rnorm(n=n, mean=mean, sd=sd)
# 	}
# 	rnorm.vect <- Vectorize(rnorm.vect, vectorize.args=c('mean', 'sd'))
# 	
# 	# allocate storage for simulated horizon depths
# 	l <- list()
# 	
# 	# generate n-simulated horizon depths
# 	for(i in 1:n) {
# 		# simulate
# 		s <- mapply(rnorm.vect, thick, hz.sd, n=iterations)
# 		
# 		# sample a single value per horizon, and use as the representative value
# 		s <- round(apply(s, 2, sample, size=1))
# 		
# 		# convert thickness values that are below min.thick -> min.thick
# 		s <- pmax(s, min.thick)
# 		
# 		# convert thickness -> depths
# 		s <- cumsum(s)
# 		# use a ID column name that isn't likely to conflict with existing column names
# 		d <- data.frame(.new_id=i, top=c(0, s[-length(s)]), bottom=s)
# 		
# 		# combine with original horizon data, and save to list element
# 		l[[i]] <- cbind(d, h)
# 	}
# 	
# 	# convert list -> data.frame
# 	x.s <- do.call(rbind, l)
# 	
# 	# upgrade to SoilProfileCollection
# 	depths(x.s) <- .new_id ~ top + bottom
# 	
#   # copy over depth units
# 	depth_units(x.s) <- depth_units(x)
# 	
# 	# move old ID into @site
# 	fm <- as.formula(paste0('~ ', old.id.name))
# 	site(x.s) <- fm
# 	
# 	# done
# 	return(x.s)
}
