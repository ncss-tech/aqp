## Representative O horizon colors from NASIS pedons
## 2026-03-18
## D.E. Beaudette


# GH:soil-color/moist-dry-model/representative-O-hz-colors.R

# 2026-03-18 update
#
#    Oi    Oe    Oa other 
#  1751  1493  1635   366 



Ohz.colors <- structure(
  list(
    state = c("dry", "dry", "dry", "dry", "moist", "moist", "moist", "moist"), 
    genhz = c("Oi", "Oe", "Oa", "other", "Oi", "Oe", "Oa", "other"), 
    L1.munsell = c("10YR 4/1", "10YR 4/1", "N 3/0", "10YR 4/1", "10YR 2.5/1", "10YR 2.5/1", "N 2.5/0", "10YR 3/1"
    ), 
    group.dE00 = c(8.72804445459893, 7.74452801240629, 8.09474411945683, 
                   9.20930707790642, 5.79321325735549, 4.98447184112642, 
                   3.01138533033395, 6.84696780188908)
  ), 
  row.names = c(NA, -8L), class = "data.frame")



save(Ohz.colors, file = 'data/Ohz-colors.rda')


