library(dendextend)
library(aqp)
library(cluster)
library(ape)

library(latticeExtra)
library(viridis)

data("ROSETTA.centroids")

# local copy
x <- ROSETTA.centroids

# cluster based on parameters
m <- ROSETTA.centroids[, c('sat', 'fc', 'pwp')]
row.names(m) <- x$texture
h <- as.hclust(agnes(daisy(m)))

# attemp to align with total AWC
h <- dendextend::rotate(h, order(x$awc))
h2 <- as.phylo(h)

# check: OK!
plot(h2)
h$labels[h$order]

# several sorting strategies

# clustering approach
x$texture_sort1 <- factor(x$texture, levels = rev(h$labels[h$order]))

# just AWC
x$texture_sort2 <- factor(x$texture, levels = x$texture[order(x$awc)])


segplot(texture ~ pwp + fc, data=x, 
        horizontal = TRUE, 
        level = awc, col.regions=viridis,
        main='Available Water Holding Capacity\nUSDA-ARS ROSETTA Model Centroids', 
        sub='Sorted According to Field Book v3.0',
        xlab=expression(Volumetric~Water~Content~~(cm^3/cm^3))
)

segplot(texture_sort1 ~ pwp + fc, data=x, 
        horizontal = TRUE, level = awc, col.regions=viridis,
        main='Available Water Holding Capacity\nUSDA-ARS ROSETTA Model Centroids',
        sub='Sorted According to {PWP, FC, SAT} Values',
        xlab=expression(Volumetric~Water~Content~~(cm^3/cm^3))
)

segplot(texture_sort2 ~ pwp + fc, data=x, horizontal = TRUE, 
        level = awc, col.regions=viridis,
        main='Available Water Holding Capacity\nUSDA-ARS ROSETTA Model Centroids',
        sub='Sorted According to AWC',
        xlab=expression(Volumetric~Water~Content~~(cm^3/cm^3))
        )

