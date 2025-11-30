### Overview

CLABSIalertR provides tools for monitoring central line–associated bloodstream infections (CLABSIs) using Poisson-based u-charts and Statistical Process Control (SPC) rules.
It is designed for Infection Preventionists who want early-warning detection of unusual variation in CLABSI rates using monthly line-days and event counts.

The package was developed as the final project for:

### Getting Started

The official release of this package is hosted on GitHub at:
https://github.com/sparenas/CLABSIalertR

To install the package from source in an R session:

#### from your local .tar.gz file
install.packages("CLABSIalertR_0.0.0.9000.tar.gz",
                 repos = NULL, type = "source")


#### Or install directly from GitHub (if you enable devtools):

devtools::install_github("sparenas/CLABSIalertR")


After installation, load the package with:

library(CLABSIalertR)

For further details about how to use the package, read the vignette that comes with it.

If you cannot build the package manually, the GitHub method above is recommended.

Feel free to report issues, request features, or contribute improvements through the GitHub repository.
