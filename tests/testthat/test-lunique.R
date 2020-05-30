library(aqp)
library(testthat)
data(sp5)

# ensure that first 9 profiles, in order, are returned
input <- lapply(list(c(1,2,3), c(2,3,4), c(5,6,7), c(8,9,1)), function(idx) sp5[idx,])
output <- lunique(input)
expect_equal(do.call('c', lapply(output, function(x) {
  match(profile_id(x), profile_id(sp5))
})), 1:9)
expect_equal(match(profile_id(aqp::union(output)), profile_id(sp5)), 1:9)

# check NA handling
input <- lapply(list(c(1,NA,3), c(2,3,4), NA, c(8,9,1)), function(idx) {
  if(all(is.na(idx)))
    return(NA)
  sp5[idx[which(!is.na(idx))],]
})
output <- lunique(input)

# profiles 1 and 3 from first set
expect_true(all(c("soil1", "soil100") %in% profile_id(output[[1]])))

# profiles 2 and 4 from second set
expect_true(all(c("soil10", "soil101") %in% profile_id(output[[2]])))

# profiles 8 and 9 from fourth set
expect_true(all(c("soil105", "soil106") %in% profile_id(output[[4]])))

# total of four sets
expect_equal(length(output), 4)

# of which the third contains NA
expect_equal(which(is.na(output)), 3)

# we get a warning when unioning, but it is successful
expect_message(res <- length(profile_id(aqp::union(output))), 
                        regexp = "union: one or more input list elements is NA")

# the resulting SoilProfileCollection has 6 profiles
expect_equal(res, 6)

