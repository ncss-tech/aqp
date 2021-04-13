library(aqp)

data("munsell.spectra.wide")


m <- '5B 6/10'
x <- munsell.spectra.wide[, m]

spec2Munsell(x)

cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
plotColorMixture(cols, swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(cols, keepMixedSpec = TRUE)
spec2Munsell(res$spec)


plotColorMixture(c('10YR 6/2', '5YR 5/6'), w = c(2,1), swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(c('10YR 6/2', '5YR 5/6'), w = c(2,1), keepMixedSpec = TRUE)
spec2Munsell(res$spec)

colorContrastPlot(m1 = '5Y 6/3', m2 = '7.5YR 6/4')



# # interpolate reflectance spectra to 1nm
# R <- data.frame(
#   wlmn = 380:730, 
#   intensity = f(380:730)
# )
# 
# # for plotting
# D65$scaled.intensity <- D65$intensity / 100
# 
# matplot(CM[, 1], CM[2:4], lty = 1, type = 'l', las = 1, col = c('red', 'green', 'blue'))
# lines(D65$wlnm, D65$scaled.intensity, type = 'l', lty = 2)
# lines(R, type = 'l', col = parseMunsell(res$mixed$munsell), lwd = 2)
# 
# # reflectance spectra * illuminant
# S <- R$intensity * D65$intensity
# 
# A <- CM[, -1]
# xyz <- t(A) %*% S
# xyz <- t(xyz)[1, ]
# 
# k <- t(A) %*% D65$intensity
# k <- t(k)[1, ]
# 
# col.xyz <- xyz / k[2]
# 
# ## doesn't work
# convert_colour(rbind(c(0.1828, 0.1844, 0.3806)), from = 'xyz', to = 'rgb', white_from = 'D65', white_to = 'D65')
# convertColor(rbind(c(0.1828, 0.1844, 0.3806)), from = 'XYZ', to = 'sRGB', from.ref.white = 'D65', to.ref.white = 'D65')
# 
# # this does!
# ()
# 
# rgb2munsell(col.rgb)
# 
# 
# # 
# # 
# # xyz <- xyz / xyz[2]
# 

