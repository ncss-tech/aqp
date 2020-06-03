library(aqp)
library(data.table)
library(tibble)

###
### SETUP - switch statement to select test object class
###
###  use: class to use 
###       data.frame [1], data.table [2], or tbl_df [3]
### 
     use <- 3

     use_class <- switch(as.character(use),
                         "1" = "data.frame", 
                         "2" = "data.table", 
                         "3" = "tbl_df")
     
#####################################################################

# helper function: vector concatenation
cc <- function(l) do.call('c', as.list(l))

# wrapper function for converting between:
#  data.frame, data.table and tbl_df
test_object <- function(object, use_class) {
  aqp:::.as.data.frame.aqp(object, as.class = use_class)
}

# create test data
df <- data.frame(id = cc(lapply(1:4, function(i) rep(i, 10))),
                top = cc(rep(0:9, 4)), bottom = cc(rep(1:10, 4)),
                siteprop = 8, prop = 18)

# create test object from test data data.frame
test <- test_object(object = df, use_class = use_class)

# promote to SPC
depths(test) <- id ~ top + bottom

# normalize a site-level attribute
site(test) <- ~ siteprop

# inspect site table
site(test)

# add some site data, for only two sites
site(test) <- test_object(data.frame(id = as.character(2:3), 
                                     siteclass = state.abb[1:2]), use_class)

# inspect site table with new variable (some NA)
site(test)

# inspect horizon table
horizons(test)

# # test $ [[ setter/getter
horizons(test)$foo <- 100

all(horizons(test)[['foo']] == horizons(test)$foo)

new <- rep(200, nrow(test))
test[["foo"]] <- new
all(horizons(test)$foo / 2 == (new - 100))

# add some horizon data
value <-  test_object(data.frame(id = c(rep(1,10),rep(2,10),3),
                                 hzID = 1:21,
                                 hzclass = letters[1:21]), use_class)

horizons(test) <- value

# horizons in order
horizons(test)[15:25,]

# this time we have given an impossible condition: id=2 hzid=21
value <-  test_object(data.frame(id = c(2,2),
                                 hzID = 20:21,
                                 another = letters[1:2]), use_class)

horizons(test) <- value
horizons(test)[15:25,]

# this is fun. we can do joins without hzID too
value <-  test_object(data.frame(id = 2,
                                 almostdone = letters[3]), use_class)

horizons(test) <- value

# this time we applied to all horizons in profile 2
horizons(test)[9:21,]

# this is fun. we can do joins without any id at all
value <-  test_object(data.frame(prop = 18,
                                 done = "on the line"), use_class)
horizons(test) <- value

# this time we have used a different horizon attribute (clay content)
horizons(test)

