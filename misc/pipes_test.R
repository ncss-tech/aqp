library(aqp)
library(soilDB)
library(magrittr)
library(data.table)

data("loafercreek")
metadata(loafercreek)$aqp_df_class <- "data.table"

loafercreek %>% 
    filter(!is.na(taxorder)) %>% 
    group_by(taxorder) %>% 
    summarize(clay_mean = mean(clay, na.rm = TRUE),
         clay_sd   = sd(clay,        na.rm = TRUE))
    
    # TODO: metadata being nuked by glomApply?
    # glomApply(estimatePSCS, truncate = TRUE) #%>%
    # summarize(clay_mean = mean(clay, na.rm = TRUE),
    #           clay_sd   = sd(clay,   na.rm = TRUE))
  