library(aqp)
library(cluster)

# example data
data('sp4')
# promote to SPC
depths(sp4) <- id ~ top + bottom


# depth-wise comparison of hz-level attributes to 50cm
d <- profile_compare(sp4, vars=c('ex_Ca_to_Mg', 'K', 'sand'), k=0, max_d=50)

# k-medoids clustering into 3 groups
clust <- pam(x = d, diss = TRUE, k = 3)

# save clustering + ID to data.frame
clust.df <- data.frame(
  id = names(clust$clustering),
  cluster = factor(clust$clustering),
  stringsAsFactors = FALSE
)

# just to be safe, make sure profile ID name is correct
names(clust.df)[1] <- idname(sp4)

# safely merge into @site via left join on idname(sp4)
site(sp4) <- clust.df

# check
par(mar=c(0,0,3,1))
groupedProfilePlot(sp4, groups = 'cluster', color = 'ex_Ca_to_Mg')
groupedProfilePlot(sp4, groups = 'cluster', color = 'K')
groupedProfilePlot(sp4, groups = 'cluster', color = 'sand')
