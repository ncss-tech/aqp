
random_profile <- function(
id,
n=c(3,4,5,6), 
min_thick=5, 
max_thick=30, 
n_prop=5,
exact=FALSE
)
{

# sanity check
if(missing(id))
	stop('must specify an id')

if(max_thick < min_thick)
	stop('illogical horizon thickness constraints')

# if requested, give back the exact number of horizons
if(length(n) == 1 & exact)
	n_hz <- n

# otherwise randomly choose from suggestions
else
	n_hz <- sample(n, 1)

# generate hz top bnd
tops <- integer(n_hz-1)
for(i in 1:(n_hz-1))
	tops[i] <- sample(min_thick:max_thick, 1)

# add 0, then generate bottom bnd
tops <- as.integer(c(0, tops))
bottoms <- as.integer(c(tops[-1], sample(min_thick:max_thick, 1)))

# combine into a df
d <- data.frame(id=id, top=cumsum(tops), bottom=cumsum(bottoms), name=paste('H',1:n_hz,sep=''))

# generate several properties
# with different means / sd
for(i in 1:n_prop)
	{
	p <- numeric(n_hz)
	p[1] <- rnorm(1)
	for(j in 2:n_hz)
		p[j] <- p[j-1] + rnorm(1, mean=runif(n=1, min=-10, max=10), sd=runif(n=1, min=1, max=10))

	# add properties
	new_col <- paste('p',i, sep='')
	d[,new_col] <- p
	}

# all done
return(d)
}

