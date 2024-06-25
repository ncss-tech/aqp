

.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is aqp ", utils::packageDescription("aqp", field="Version"), appendLF = TRUE)
}
