context("plotSPC")

data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# additional example data
set.seed(101010)
p <- lapply(letters[1:10], random_profile, method = 'LPP', SPC = TRUE)
p <- combine(p)

# make a factor style hz attr
p$p.factor <- cut(p$p1, quantile(p$p1), include.lowest = TRUE)

# note: hzdesgnname() returns "" when metadata are missing

## tests

test_that("plotSPC: aqp.env settings", {

  # explainer
  explainPlotSPC(sp1)

  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)

  # should be a list
  expect_true(is.list(lsp))

  # check for required components
  expect_equal(names(lsp), c("width", "plot.order", "x0", "pIDs", "idname", "y.offset", "scaling.factor",
                             "max.depth", "n", "extra_x_space", "extra_y_space"))

  # basic integrity checks
  expect_equal(profile_id(sp1), lsp$pIDs)
  expect_equal(idname(sp1), lsp$idname)
  expect_true(length(sp1) == lsp$n)
  expect_equal(1:length(sp1), lsp$plot.order)
})


test_that("plotSPC: figure settings", {

  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, scaling.factor=0.5, width=0.8,
                        y.offset=8, n=15, max.depth = 100)

  # check adjustments
  expect_equal(lsp$scaling.factor, 0.5)
  expect_equal(lsp$width, 0.8)
  expect_equal(lsp$y.offset, rep(8, times = length(sp1)))
  expect_equal(lsp$n, 15)
  expect_equal(lsp$max.depth, 100)
})


test_that("plotSPC: re-ordering of profiles", {

  # re-order
  new.order <- c(1,3,5,7,9,2,4,6,8)
  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, plot.order=new.order)

  # profile IDs should be sorted according to plot.order
  expect_equal(profile_id(sp1)[new.order], lsp$pIDs)

  # plotting order should be saved
  expect_equal(new.order, lsp$plot.order)
})

# output is purely graphical, testing to make sure no errors / warnings are generated
test_that("plotSPC: shrinking horizon names", {
  
  # make sure these work
  plotSPC(sp1, name.style = 'center-center', shrink.thin = NULL, shrink = TRUE, cex.names = 0.8)
  plotSPC(sp1, name.style = 'center-center', shrink.thin = 5, shrink = TRUE, cex.names = 0.8)
  # test
  lsp <- explainPlotSPC(sp1, name.style = 'center-center', shrink.thin = 15, shrink = TRUE, cex.names = 0.8)
  expect_true(is.list(lsp))
})



test_that("plotSPC: relative spacing", {

  # relative positions
  x.pos <- c(1+0, 2+0, 3+0, 4+0.1, 5+0.1, 6+0.1, 7+0.2, 8+0.2, 9+0.2)
  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, relative.pos = x.pos)

  # plot order not affected
  expect_equal(lsp$plot.order, 1:length(sp1))

  # x0 adjusted as expected
  # 1:length(x) + offsets
  expect_equal(lsp$x0, x.pos)
})



test_that("plotSPC: re-ordering of profiles and relative spacing", {

  # new order
  new.order <- c(1,3,5,7,9,2,4,6,8)
  # relative positions: in the new ordering
  x.pos <- c(1+0, 2+0, 3+0, 4+0.1, 5+0.1, 6+0.1, 7+0.2, 8+0.2, 9+0.2)

  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, plot.order=new.order, relative.pos = x.pos)

  # profile IDs should be sorted according to plot.order
  expect_equal(profile_id(sp1)[new.order], lsp$pIDs)

  # plotting order should be saved
  expect_equal(new.order, lsp$plot.order)

  # x0 adjusted as expected
  expect_equal(lsp$x0, x.pos)
})


test_that("plotSPC: re-ordering via relative spacing", {

  # re-order by adjusting the relative positions
  x.pos <- length(sp1):1

  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, relative.pos = x.pos)

  # plotting order / IDs are not modified!
  expect_equal(profile_id(sp1), lsp$pIDs)
  expect_equal(lsp$plot.order, 1:length(sp1))

  # x0 adjusted as expected
  expect_equal(lsp$x0, x.pos)
})

test_that("addBracket works", {

  expect_silent(addBracket(data.frame(id = profile_id(sp1), label="bar", top = 0, bottom = 25)))

  expect_silent(addBracket(data.frame(id = profile_id(sp1), top = 0, bottom = NA)))

  diagnostic_hz(sp1) <- data.frame(id = profile_id(sp1), featkind = "foo", featdept = 0, featdepb = 50)

  expect_silent(addDiagnosticBracket(sp1, kind =  "foo"))

})

test_that("addVolumeFraction works", {

  # does it work with default arguments?
  plotSPC(sp1, name = 'name')

  expect_silent(addVolumeFraction(sp1, 'prop'))

  # additional arguments
  expect_silent(addVolumeFraction(sp1, 'prop', res = 5))

  expect_silent(addVolumeFraction(sp1, 'prop', cex.min = 0.5, cex.max = 2))

  expect_silent(addVolumeFraction(sp1, 'prop', pch = 15))

  # color specification is important:
  # single color
  expect_silent(addVolumeFraction(sp1, 'prop', col = 'red'))

  # or vector of colors, must be same length as nrow(x)
  expect_silent(addVolumeFraction(sp1, 'prop', col = rep('green', times=nrow(sp1))))

  sp1$prop1k <- sp1$prop / 1000
  # message due to values < 1
  expect_message(addVolumeFraction(sp1, 'prop1k'),
                 "all prop1k values are < 0.5, likely a fraction vs. percent")

  sp1$prop1k <- sp1$prop * 1000
  # warnings due to values >100
  expect_warning(addVolumeFraction(sp1, 'prop1k'))

})


test_that("addVolumeFraction expected errors", {

  plotSPC(sp1, name='name')


  # bad column name
  expect_error(addVolumeFraction(sp1, 'prop1'))

  # incorrectly specified colors
  expect_error(addVolumeFraction(sp1, 'prop', col = c('red', 'green')))

})

# https://github.com/ncss-tech/aqp/issues/8
test_that("addVolumeFraction fractional horizon depths", {

  plotSPC(sp1, name='name')


  # modify depths
  sp1$top[4] <- sp1$top[4] + 0.5
  sp1$bottom[3] <- sp1$top[4]

  # fractional horizon depths
  expect_message(addVolumeFraction(sp1, 'prop'), regexp = 'truncating')

})


test_that("horizon color specification interpreted correctly", {
  
  # this function works with contents of @horizons
  h <- horizons(p)
  
  # colors to use
  cols <- c("#5E4FA2", "#3288BD", "#66C2A5","#ABDDA4", "#E6F598", "#FEE08B","#FDAE61", "#F46D43", "#D53E4F","#9E0142")
  
  # attempt to interpret
  x <- aqp:::.interpretHorizonColor(
    h, 
    color = 'p1', 
    default.color = 'grey', 
    col.palette = cols,
    col.palette.bias = 1,
    n.legend = 8
    )
  
  # reasonable object?
  expect_true(inherits(x, 'list'))
  expect_true(length(x) == 2)
  expect_true(length(x$color.legend.data) == 4)
  expect_false(x$color.legend.data$multi.row.legend)
  expect_null(x$color.legend.data$leg.row.indices)
  
  # another try, no colors specified
  x <- aqp:::.interpretHorizonColor(
    h, 
    color = NA, 
    default.color = 'grey', 
    col.palette = cols,
    col.palette.bias = 1,
    n.legend = 8
  )
  
  # horizon colors should match default.color
  expect_true(all(x$colors == 'grey'))
  
  # there is no legend
  expect_true(is.null(x$color.legend.data))
  
  
  # factor variable + multi-line legend
  x <- aqp:::.interpretHorizonColor(
    h, 
    color = 'p.factor', 
    default.color = 'grey', 
    col.palette = cols,
    col.palette.bias = 1,
    n.legend = 2
  )
  
  # multi-line legend details
  expect_true(x$color.legend.data$multi.row.legend)
  # row indices are stored in a list
  expect_true(inherits(x$color.legend.data$leg.row.indices, 'list'))
  # there should be two rows
  expect_true(length(x$color.legend.data$leg.row.indices) == 2)
  # legend item indices are stored by row 
  expect_equal(x$color.legend.data$leg.row.indices[[1]], c(1,2))
  expect_equal(x$color.legend.data$leg.row.indices[[2]], c(3,4))
  
})







