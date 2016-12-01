# Code to create the R package
# setwd("/Users/TAEHEEJUNG/Desktop/STAT243_HW/v2_new/mfaMKTLT")

# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)
library(roxygen2)
library(testthat)
library(knitr)

# Save the wine data
#   NB: this assumes we are in the package directory, which is likely
#   NOT the directory where this mkpkg.R file lives
load("../../../pkg_data/winedata.Rdata")
devtools::use_data(winedata, overwrite = TRUE)
rm(winedata)

# creating documentation (i.e. the Rd files in man/)
devtools::document()

# checking documentation
devtools::check_man()

# running tests
devtools::test()

# vignettes
devtools::build_vignettes()

# building tarball (e.g. oski_0.1.tar.gz)
devtools::build()

# checking install
devtools::install()
