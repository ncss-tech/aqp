library(aqp)
library(daff)

s1 <- readRDS('misc/slab/slab-1.x-kssl.rds')
s2 <- readRDS('misc/slab/slab-2.x-kssl.rds')


str(s1)
str(s2)

head(s1)
head(s2)

s1 <- s1[order(s1$taxonname, s1$variable, s1$top), ]
s2 <- s2[order(s2$taxonname, s2$variable, s2$top), ]

head(s1)
head(s2)

d <- diff_data(s1, s2)

daff::render_diff(d)
