library(aqp)
library(daff)

# valid profiles
s1 <- readRDS('misc/slab/slab-1.x-kssl-valid.rds')
s2 <- readRDS('misc/slab/slab-2.x-kssl-valid.rds')

str(s1)
str(s2)

head(s1)
head(s2)

s1 <- s1[order(s1$taxonname, s1$variable, s1$top), ]
s2 <- s2[order(s2$taxonname, s2$variable, s2$top), ]

head(s1)
head(s2)

# identical (1 reordered column)
d <- diff_data(s1, s2) |>
  render_diff()

# all profiles
# re-order rows, use common column ordering
s1 <- readRDS('misc/slab/slab-1.x-kssl.rds')
s2 <- readRDS('misc/slab/slab-2.x-kssl.rds')
s1 <- s1[order(s1$taxonname, s1$variable, s1$top), names(s1)]
s2 <- s2[order(s2$taxonname, s2$variable, s2$top), names(s1)]

# chaix is identical
d1 <- subset(s1, taxonname == 'chaix')
d2 <- subset(s2, taxonname == 'chaix')
d12 <- diff_data(d1,d2) |> render_diff()

# holland is identical
d1 <- subset(s1, taxonname == 'holland')
d2 <- subset(s2, taxonname == 'holland')
d12 <- diff_data(d1, d2) |> render_diff()

# musick reorders 231 rows, modifies 205 rows in 6 columns
# numeric differencesin musick data
d1 <- subset(s1, taxonname == 'musick')
d2 <- subset(s2, taxonname == 'musick')
d12 <- diff_data(d1, d2) |> render_diff()

# 109 rows differ in "wmpd"
d1 <- subset(s1, taxonname == 'musick' & variable == "wmpd")
d2 <- subset(s2, taxonname == 'musick' & variable == "wmpd")
d12 <- diff_data(d1,d2) |> render_diff()

# 109 rows differ in "clay"
d1 <- subset(s1, taxonname == 'musick' & variable == "clay")
d2 <- subset(s2, taxonname == 'musick' & variable == "clay")
d12 <- diff_data(d1, d2) |> render_diff()

# 109 rows differ in "estimated_ph_h2o"
d1 <- subset(s1, taxonname == 'musick' & variable == "estimated_ph_h2o")
d2 <- subset(s2, taxonname == 'musick' & variable == "estimated_ph_h2o")
d12 <- diff_data(d1, d2) |> render_diff()

# 109 rows differ in "bs82"
d1 <- subset(s1, taxonname == 'musick' & variable == "bs82")
d2 <- subset(s2, taxonname == 'musick' & variable == "bs82")
d12 <- diff_data(d1, d2) |> render_diff()


