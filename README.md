
# project3package

<!-- badges: start -->
[![R-CMD-check](https://github.com/anikalindley/project3package/workflows/R-CMD-check/badge.svg)](https://github.com/anikalindley/project3package/actions)

[![codecov](https://codecov.io/gh/anikalindley/project3package/branch/master/graph/badge.svg?token=PO94TB7VAU)](https://codecov.io/gh/anikalindley/project3package)

<!-- badges: end -->

The goal of project3package is to develop a well-documented, well-tested, and well-explained R package. 

## Installation

You can install the package through GitHub using:
``` r
devtools::install_github("anikalindley/project3package")
library(project3package)
```

You can view the vignette here: 
```{r}
devtools::install_github("anikalindley/project3package", build_vignette = TRUE, build_opts = c())

library(project3package)

# Use this to view the vignette in the Demo HTML help
help(package = "Demo", help_type = "html")

# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "Demo")
```

