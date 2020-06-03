library(aqp)
library(soilDB)
library(magrittr)
library(data.table)

data("loafercreek")
metadata(loafercreek)$aqp_df_class <- "data.table"
metadata(loafercreek)$.spc_group_by <- "taxorder"
res <- loafercreek %>% 
    filter(!is.na(taxorder)) %>% 
    group_by(taxorder) %>%
    glomApply(estimatePSCS, truncate = TRUE)

res %>%  summarize(clay_mean = mean(clay, na.rm = TRUE),
              clay_sd   = sd(clay,   na.rm = TRUE))
    
# TODO: metadata being nuked by glomApply?