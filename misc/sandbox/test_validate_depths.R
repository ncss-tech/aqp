# read in horizons from local database from fetchNASIS(from = "pedon_report")
h <- get_hz_data_from_NASIS_db(SS = FALSE)
h$peiid <- as.character(h$peiid)
h2 <- h
depths(h2) <- peiid ~ hzdept + hzdepb


# checkHzDepthLogic ----
test_chl <- checkHzDepthLogic(h2)
test_chl$idx <- with(test_chl, paste(peiid, valid, depthLogic, sameDepth, missingDepth, overlapOrGap))


# validate_depths ----
# has options for both data.frame and SoilProfileCollection
test_val <- validate_depths(h)
test_val <- horizons(validate_depths(h2))

# reformat horizon results to match checkHzDepthLogic
vars <- names(test_val)[grepl("^valid_|^peiid$", names(test_val))]
test_val_rf  <- aggregate(
  cbind(valid_dep_all, valid_dep_order, valid_dep_diff, valid_dep_complete, valid_dep_overlap) ~ peiid, 
  data = test_val[vars], 
  FUN = function(x) all(x, na.rm = TRUE), 
  na.action = na.pass
  )

names(test_val_rf)[-1] <- c("valid", "depthLogic", "sameDepth", "missingDepth", "overlapOrGap")
test_val_rf[3:6] <- !test_val_rf[3:6]
test_val_rf$idx <- with(test_val_rf, paste(peiid, valid, depthLogic, sameDepth, missingDepth, overlapOrGap))


# compare results ----
idx <- test_chl$valid == test_val_rf$valid
sum(idx) # both match, but test_chl applies overlapping logic checks


idx2 <- test_chl$idx != test_val_rf$idx
View(test_chl[idx2, ]) # test_chl checks if the individual pedons are sorted, applies overlapping logic
View(test_val_rf[idx2, ])


daff::render_diff(daff::diff_data(test_chl, test_val_rf))

