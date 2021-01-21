# 
#' Simulate Soil Profiles
#' 
#' @description Simulate a collection of soil profiles based on the horizonation of a single soil profile. 
#' 
#' The function is most useful when supplied with a meaningful standard deviation in thickness for each horizon -- which generally implies an aggregation of **several** profiles (say, using some generalized horizon pattern).
#' 
#' This contrasts with a similar approach in [permute_profile()] -- which "perturbs" the **boundary between horizons** using a standard deviation of "horizon transition zone" thickness. This thickness standard deviation corresponds roughly to the concept of "horizon boundary distinctness."
#'
#' @param x a SoilProfileCollection object containing a single profile from which to draw simulated data
#' @param n the number of requested simulations
#' @param iterations sampling iterations used to determine each horizon thickness
#' @param hz.sd standard deviation used to simulate horizon thickness, can be a vector but must divide evenly into the number of horizons found in \code{x}
#' @param min.thick minimum horizon thickness allowed in simulation results
#'
#' @return A SoilProfileCollection object with \code{n} simulated profiles, each containing the same number of horizons and same data as \code{x}
#' 
#' @details This function generates a collection of simulated soil profiles based on the horizon thickness data associated with a single "template" profile. Simulation is based on sampling from a family of gaussian distribution with means defined by the "template" profile and standard deviation defined by the user.
#' 
#' @export
#' 
#' @seealso \code{\link{random_profile}}  \code{\link{permute_profile}}
#' 
#' @author D. E. Beaudette
#' 
#' @examples
#' 
#' # load sample data and convert into SoilProfileCollection
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#' 
#' # select a profile to use as the basis for simulation
#' s <- sp3[3,]
#' 
#' # reset horizon names
#' s$name <- paste('H', seq_along(s$name), sep = '')
#' 
#' # simulate 25 new profiles, using 's' and function defaults
#' sim.1 <- sim(s, n = 25)
#' 
#' # simulate 25 new profiles using 's' and variable SD for each horizon
#' sim.2 <- sim(s, n = 25, hz.sd = c(1, 2, 5, 5, 5, 10, 3))
#' 
#' # plot
#' par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
#' plot(sim.1)
#' mtext(
#'   'SD = 2',
#'   side = 2,
#'   line = -1.5,
#'   font = 2,
#'   cex = 0.75
#' )
#' plot(sim.2)
#' mtext(
#'   'SD = c(1, 2, 5, 5, 5, 10, 3)',
#'   side = 2,
#'   line = -1.5,
#'   font = 2,
#'   cex = 0.75
#' )
#' 
#' # aggregate horizonation of simulated data
#' # note: set class_prob_mode=2 as profiles were not defined to a constant depth
#' sim.2$name <- factor(sim.2$name)
#' a <- slab(sim.2, ~ name, class_prob_mode = 2)
#' 
#' # convert to long format for plotting simplicity
#' library(data.table)
#' a.long <-
#'   melt(a,
#'        id.vars = c('top', 'bottom'),
#'        measure.vars = levels(sim.2$name))
#' 
#' # plot horizon probabilities derived from simulated data
#' # dashed lines are the original horizon boundaries
#' library(lattice)
#' 
#' xyplot(
#'   top ~ value,
#'   groups = variable,
#'   data = a.long,
#'   subset = value > 0,
#'   ylim = c(100,-5),
#'   type = c('l', 'g'),
#'   asp = 1.5,
#'   ylab = 'Depth (cm)',
#'   xlab = 'Probability',
#'   auto.key = list(
#'     columns = 4,
#'     lines = TRUE,
#'     points = FALSE
#'   ),
#'   panel = function(...) {
#'     panel.xyplot(...)
#'     panel.abline(h = s$top, lty = 2, lwd = 2)
#'   }
#' )
sim <- function(x, n=1, iterations=25, hz.sd=2, min.thick=2) {	
	
  hd <- horizonDepths(x)
	h <- horizons(x)
	thick <- h[[hd[2]]] - h[[hd[1]]]
	
	# remove original depth columns
	h[[hd[1]]] <- NULL
	h[[hd[2]]] <- NULL
	
	# remove horizon ID so as not to create conflicts later on
	h[[hzidname(x)]] <- NULL
	
	# keep track of old id
	old.id.name <- idname(x)
	
	# sanity checks
	if(length(x) > 1)
		stop('this function can only simulate data from a SoilProfileCollection containing a single profile')
	
	if(length(thick) %% length(hz.sd) != 0)
		stop('the length of hz.sd must divide evenly into the number of horizons', call.=FALSE)
	
	# define function to wrap rnorm for use with outer
	rnorm.vect <- function(mean, sd, n) {
		rnorm(n=n, mean=mean, sd=sd)
	}
	rnorm.vect <- Vectorize(rnorm.vect, vectorize.args=c('mean', 'sd'))
	
	# allocate storage for simulated horizon depths
	l <- list()
	
	# generate n-simulated horizon depths
	for(i in 1:n) {
		# simulate
		s <- mapply(rnorm.vect, thick, hz.sd, n=iterations)
		
		# sample a single value per horizon, and use as the representative value
		s <- round(apply(s, 2, sample, size=1))
		
		# convert thickness values that are below min.thick -> min.thick
		s <- pmax(s, min.thick)
		
		# convert thickness -> depths
		s <- cumsum(s)
		# use a ID column name that isn't likely to conflict with existing column names
		d <- data.frame(.new_id=i, top=c(0, s[-length(s)]), bottom=s)
		
		# combine with original horizon data, and save to list element
		l[[i]] <- cbind(d, h)
	}
	
	# convert list -> data.frame
	x.s <- do.call(rbind, l)
	
	# upgrade to SoilProfileCollection
	depths(x.s) <- .new_id ~ top + bottom
	
  # copy over depth units
	depth_units(x.s) <- depth_units(x)
	
	# move old ID into @site
	fm <- as.formula(paste0('~ ', old.id.name))
	site(x.s) <- fm
	
	# done
	return(x.s)
}
