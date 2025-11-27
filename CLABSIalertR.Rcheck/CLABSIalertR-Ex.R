pkgname <- "CLABSIalertR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('CLABSIalertR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_dispersion")
### * check_dispersion

flush(stderr()); flush(stdout())

### Name: check_dispersion
### Title: Check dispersion of CLABSI counts
### Aliases: check_dispersion

### ** Examples

df <- data.frame(clabsi = c(1,0,0,2,1,3,0,0,1,0,2,4))
check_dispersion(df)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
