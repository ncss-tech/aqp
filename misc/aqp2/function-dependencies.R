# functions
aqp.functions <- getNamespaceExports("aqp")
aqp.functions <- aqp.functions[-grep("_[TC]_", aqp.functions)]
aqp.grp <- vector('list')

# plotting functions
plot.idx <- grep("[Pp]lot|add|find|fix|segments", aqp.functions)
aqp.grp[['plot']] <- sort(aqp.functions[plot.idx])
aqp.functions <- aqp.functions[-plot.idx]

# soil color
color.idx <- grep("[Cc]olor|red|dark|melan|rubif|[Mm]unsell|rgb|contrast|hue|buntley|Palette", aqp.functions)
aqp.grp[['color']] <- sort(aqp.functions[color.idx])
aqp.functions <- aqp.functions[-color.idx]

# soil texture
texture.idx <- grep("tex", aqp.functions)
aqp.grp[['texture']] <- sort(aqp.functions[texture.idx])
aqp.functions <- aqp.functions[-texture.idx]

# soilprofilecollection operations
spccalc.idx <- grep("sim|get|estim|clay|mollic", aqp.functions)
aqp.grp[['spccalc']] <- sort(aqp.functions[spccalc.idx])
aqp.functions <- aqp.functions[-spccalc.idx]

spc.idx <- grep("^l?un|split|SPC|spc|[Hh]orizon|[Ss]ite|subset|filter|mutate|[Pp]rofile|Apply|glom|<-|[Hh]z|id|length|nrow|min|max|[Dd]epthOf|coord|proj|denorm|meta|restrict|aqp|check|guess", aqp.functions)
aqp.grp[['spc']] <- sort(aqp.functions[spc.idx])
aqp.functions <- aqp.functions[-spc.idx]

# aggregate profiles
spc.agg.idx <- grep("aggregate|eval|slice|[Ss]lab|group|summarize|pc", aqp.functions)
aqp.grp[['spcagg']] <- sort(aqp.functions[spc.agg.idx])
aqp.functions <- aqp.functions[-spc.agg.idx]

# xrd
xrd.idx <- grep("noise|twotheta", aqp.functions)
aqp.grp[['xrd']] <- sort(aqp.functions[xrd.idx])
aqp.functions <- aqp.functions[-xrd.idx]

# classification
tau.idx <- grep("[Tt]au|confusion|shannon|brier", aqp.functions)
aqp.grp[['classification']] <- sort(aqp.functions[tau.idx])
aqp.functions <- aqp.functions[-tau.idx]


lapply(aqp.grp, length)

View(aqp.grp)

# devtools::install_github("datastorm-open/DependenciesGraphs")
#
# library(DependenciesGraphs)
# library(aqp)
# deps <- funDependencies("package:aqp", "getArgillicBounds")
# plot(deps)
# # 
# edeps <- envirDependencies("package:aqp")
# plot(edeps)

library(mvbutils)
library(aqp)
library(reshape)
ixWhere <- match(c("package:aqp","package:reshape"), search())
foodweb(where = ixWhere, prune = ls("package:reshape"), descendents = FALSE)

# on usage of get in base::
res <- getNamespaceExports("base")
res[grepl("^get", res)]
# [1] "getNativeSymbolInfo"                "getOption"                         
# [3] "getNamespace"                       "getNamespaceVersion"               
# [5] "getElement"                         "get0"                              
# [7] "getDLLRegisteredRoutines.character" "getwd"                             
# [9] "gettext"                            "getTaskCallbackNames"              
# [11] "getSrcLines"                        "getNamespaceInfo"                  
# [13] "getExportedValue"                   "getDLLRegisteredRoutines.DLLInfo"  
# [15] "get"                                "gettextf"                          
# [17] "getNamespaceExports"                "geterrmessage"                     
# [19] "getRversion"                        "getConnection"                     
# [21] "getLoadedDLLs"                      "getHook"                           
# [23] "getNamespaceUsers"                  "getDLLRegisteredRoutines"          
# [25] "getAllConnections"                  "getNamespaceImports"               
# [27] "getCallingDLLe"                     "getCallingDLL"                     
# [29] "getNamespaceName"                  
length(res[grepl("^get", res)])
# [1] 29

# on usage of get in aqp::
res <- getNamespaceExports("aqp")
res[grepl("^get", res)]
# [1] "get.slice"                  "getClosestMunsellChip"      "get.increase.depths"       
# [4] "get.ml.hz"                  "getArgillicBounds"          "getSurfaceHorizonDepth"    
# [7] "getSoilDepthClass"          "getMineralSoilSurfaceDepth" "get.increase.matrix"       
# [10] "getPlowLayerDepth"          "getCambicBounds"  
length(res[grepl("^get", res)])
# [1] 11

# on usage of get in soilDB::
res <- getNamespaceExports("soilDB")
res[grepl("^get", res)]
# [1] "get_vegplot_species_from_NASIS_db"            "get_chorizon_from_NASISWebReport"            
# [3] "get_copedon_from_NASIS_db"                    "get_extended_data_from_pedon_db"             
# [5] "get_lmuaoverlap_from_NASISWebReport"          "get_mapunit_from_NASIS"                      
# [7] "get_colors_from_pedon_db"                     "get_cosoilmoist_from_SDA"                    
# [9] "get_vegplot_transect_from_NASIS_db"           "get_component_otherveg_data_from_NASIS_db"   
# [11] "getHzErrorsPedonPC"                           "get_concentrations_from_NASIS_db"            
# [13] "get_vegplot_tree_si_details_from_NASIS_db"    "get_colors_from_NASIS_db"                    
# [15] "get_comonth_from_NASIS_db"                    "get_veg_data_from_NASIS_db"                  
# [17] "get_cointerp_from_SDA"                        "get_mapunit_from_NASISWebReport"             
# [19] "getHzErrorsNASIS"                             "get_sitesoilmoist_from_NASISWebReport"       
# [21] "get_vegplot_location_from_NASIS_db"           "get_RMF_from_NASIS_db"                       
# [23] "get_mapunit_from_SDA"                         "get_vegplot_trhi_from_NASIS_db"              
# [25] "get_lablayer_data_from_NASIS_db"              "get_component_esd_data_from_NASIS_db"        
# [27] "get_veg_species_from_MT_veg_db"               "get_chorizon_from_SDA"                       
# [29] "get_lmuaoverlap_from_SDA"                     "get_component_correlation_data_from_NASIS_db"
# [31] "get_vegplot_textnote_from_NASIS_db"           "get_component_cogeomorph_data_from_NASIS_db" 
# [33] "get_cosoilmoist_from_NASIS"                   "get_projectmapunit_from_NASIS"               
# [35] "get_veg_from_MT_veg_db"                       "get_text_notes_from_NASIS_db"                
# [37] "get_hz_data_from_pedon_db"                    "get_labpedon_data_from_NASIS_db"             
# [39] "get_lmuaoverlap_from_NASIS"                   "get_component_restrictions_from_NASIS_db"    
# [41] "get_component_diaghz_from_NASIS_db"           "get_hz_data_from_NASIS_db"                   
# [43] "get_legend_from_NASIS"                        "get_component_from_NASISWebReport"           
# [45] "get_vegplot_tree_si_summary_from_NASIS_db"    "get_site_data_from_pedon_db"                 
# [47] "get_phfmp_from_NASIS_db"                      "get_project_correlation_from_NASISWebReport" 
# [49] "get_vegplot_transpecies_from_NASIS_db"        "get_extended_data_from_NASIS_db"             
# [51] "get_mutext_from_NASIS_db"                     "get_vegplot_from_NASIS_db"                   
# [53] "get_NOAA_GHCND_by_stationyear"                "get_soilseries_from_NASIS"                   
# [55] "get_NOAA_stations_nearXY"                     "get_veg_other_from_MT_veg_db"                
# [57] "get_component_data_from_NASIS_db"             "get_component_copm_data_from_NASIS_db"       
# [59] "get_component_horizon_data_from_NASIS_db"     "get_legend_from_SDA"                         
# [61] "get_site_data_from_NASIS_db"                  "get_component_from_SDA"                      
# [63] "get_veg_from_AK_Site"                         "get_veg_from_NPS_PLOTS_db"                   
# [65] "get_cosoilmoist_from_NASISWebReport"          "get_NOAA_GHCND"                            
# [67] "get_legend_from_NASISWebReport"               "get_soilseries_from_NASISWebReport"        
# [69] "get_project_from_NASISWebReport"              "get_projectmapunit_from_NASISWebReport"    
# [71] "get_projectmapunit2_from_NASISWebReport"      "get_progress_from_NASISWebReport"  
length(res[grepl("^get", res)])
# [1] 72