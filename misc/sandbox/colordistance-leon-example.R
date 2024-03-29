library(colordistance)

img <- loadImage('leon-example.jpg', sample.size = 10000,
                      lower = NULL, upper = NULL, CIELab=TRUE,
                      ref.white = "D65")

# https://cran.r-project.org/web/packages/colordistance/vignettes/color-spaces.html
plotPixels(img, color.space = "lab", ref.white = "D65", n = 10000,
                          main = "CIELab", ylim = c(-100, 100), zlim = c(-50, 100))


(hsv_hist <- getImageHist(img, bins = 2, lower = NULL, upper = NULL, hsv=TRUE, plotting = TRUE))

# https://cran.r-project.org/web/packages/colordistance/vignettes/lab-analyses.html
lab_kmeans <- getKMeanColors('leon-example.jpg', n = 7, sample.size = 10000,
                                            lower = NULL, upper = NULL, nstart=20, from='sRGB',
                                            color.space = "lab", ref.white = "D65")



img <- loadImage('fragipan-soil-color-example.png', sample.size = 10000,
                 lower = NULL, upper = NULL, CIELab=TRUE,
                 ref.white = "D65")

# https://cran.r-project.org/web/packages/colordistance/vignettes/color-spaces.html
plotPixels(img, color.space = "lab", ref.white = "D65", n = 10000,
           main = "CIELab", ylim = c(-100, 100), zlim = c(-50, 100))


(hsv_hist <- getImageHist(img, bins = 2, lower = NULL, upper = NULL, hsv=TRUE, plotting = TRUE))

# https://cran.r-project.org/web/packages/colordistance/vignettes/lab-analyses.html
lab_kmeans <- getKMeanColors('fragipan-soil-color-example.png', n = 7, sample.size = 10000,
                             lower = NULL, upper = NULL, nstart=20, from='sRGB',
                             color.space = "lab", ref.white = "D65")


aqp::col2Munsell(convertColor(lab_kmeans$centers, from = 'Lab', to = 'sRGB'))




