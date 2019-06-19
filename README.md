
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Centralised and Automated Reporting Tools <img src="man/figures/carot_hex.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/CARoT.svg?label=%22latest%20tag%22)](https://github.com/mcanouil/CARoT)
[![Travis-CI Build
Status](https://travis-ci.org/mcanouil/CARoT.svg?branch=master)](https://travis-ci.org/mcanouil/CARoT)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/mcanouil/CARoT?branch=master&svg=true)](https://ci.appveyor.com/project/mcanouil/CARoT)
[![Coverage Status
(codecov)](https://codecov.io/gh/mcanouil/CARoT/branch/master/graph/badge.svg)](https://codecov.io/gh/mcanouil/CARoT)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/CARoT)](https://cran.r-project.org/package=CARoT)
[![cran
checks\_worst](https://cranchecks.info/badges/worst/CARoT)](https://cran.r-project.org/web/checks/check_results_CARoT.html)
[![CRAN\_Download\_total](http://cranlogs.r-pkg.org/badges/grand-total/CARoT)](https://cran.r-project.org/package=CARoT)
<!--[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/2719/badge)](https://bestpractices.coreinfrastructure.org/projects/2719)-->
<!--[![cran checks_summary](https://cranchecks.info/badges/summary/CARoT)](https://cran.r-project.org/web/checks/check_results_CARoT.html)-->
<!--[![CRAN_Download_month](http://cranlogs.r-pkg.org/badges/CARoT?color=brightgreen)](https://cran.r-project.org/package=CARoT)-->
<!--[![Coverage Status (coveralls)](https://coveralls.io/repos/github/mcanouil/CARoT/badge.svg?branch=master)](https://coveralls.io/github/mcanouil/CARoT?branch=master)-->
<!-- badges: end -->

## Installation

``` r
# Install CARoT from CRAN:
install.packages("CARoT")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("mcanouil/CARoT")
```

## Overview

*CARoT* (Centralised and Automated Reporting Tools) is an under
development set of Quality-Control reporting tools and some other
functions.

Currently *CARoT* includes the following functions:

  - `estimate_ethnicity()` allows to compute the genomic component (and
    some figures) for ethnicity based on VCF files.
  - `ggheatmap()` allows to compute heatmap with dendrogram on x-axis
    and y-axis using [ggplot2](https://ggplot2.tidyverse.org/).
  - `read_idats()` allows to efficiently import idats files mostly using
    [minfi](https://doi.org/doi:10.18129/B9.bioc.minfi) functions.
  - `pca_report()` allows to compute an analysis report using principal
    component analysis from
    [flashpca](https://github.com/gabraham/flashpca) tool.  
    The function can be used in Rmarkdown chunk with `results="asis"` to
    render the report.
  - `qc_idats()` allows to compute quality-control of methylation array
    from Illumina using a default [rmarkdown
    template](inst/rmarkdown/qc_idats.Rmd).
  - `qc_plink()` allows to compute quality-control of genotyping array
    (PLINK format) using a default [rmarkdown
    template](inst/rmarkdown/qc_plink.Rmd).

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/mcanouil/CARoT/issues).  
For questions and other discussion, please contact the package
maintainer.

-----

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md).  
By participating in this project you agree to abide by its terms.
