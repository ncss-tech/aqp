# .onLoad <- function(lib, pkg)  {
#     packageStartupMessage("This is aqp ", utils::packageDescription("aqp", field="Version"), "\n", "see vignette('aqp-vignette') for the extended manual", appendLF = TRUE)
# }

.onLoad <- function(lib, pkg)  {
    packageStartupMessage("This is aqp ", utils::packageDescription("aqp", field="Version"), appendLF = TRUE)
}
