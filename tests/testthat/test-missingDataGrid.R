context("missingDataGrid")

# 10 random profiles
set.seed(10101)
s <- lapply(as.character(1:10), random_profile)
s <- do.call('rbind', s)

# randomly sprinkle some missing data
s[sample(nrow(s), 5), 'p1'] <- NA
s[sample(nrow(s), 5), 'p2'] <- NA
s[sample(nrow(s), 5), 'p3'] <- NA

# set all p4 and p5 attributes of `soil 1' to NA
s[which(s$id == '1'), 'p5'] <- NA
s[which(s$id == '1'), 'p4'] <- NA

# upgrade to SPC
depths(s) <- id ~ top + bottom



## tests


test_that("works as expected", {
  
  # run it
  m <- missingDataGrid(
    s, 
    max_depth = 100, 
    vars = c('p1', 'p2', 'p3', 'p4', 'p5'), 
    main='Missing Data Fraction'
  )
  
  # expected output
  expect_true(inherits(m, 'list'))
  expect_true(length(m) == 2)
  
  expect_true(inherits(m$fig, 'trellis'))
  expect_true(inherits(m$summary, 'data.frame'))
  
  expect_true(nrow(m$summary) == length(s))
  expect_true(ncol(m$summary) == length(vars) + 1)
  
  # select missing data proportions
  # verified
  expect_true(m$summary$p1[1] == 17)
  expect_true(m$summary$p2[1] == 0)
  expect_true(m$summary$p3[1] == 17)
  expect_true(m$summary$p4[1] == 100)
  expect_true(m$summary$p5[1] == 100)
  
  # verified
  expect_true(m$summary$p1[3] == 0)
  expect_true(m$summary$p2[3] == 40)
  expect_true(m$summary$p3[3] == 20)
  expect_true(m$summary$p4[3] == 0)
  expect_true(m$summary$p5[3] == 0)
  
})
