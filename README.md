
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Centralised and Automated Reporting Tools <img src="man/figures/carot.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![GitHub
tag](https://img.shields.io/github/tag/omicsr/CARoT.svg?label=latest%20tag&include_prereleases)](https://github.com/omicsr/CARoT)
<!-- badges: end -->

## Installation

``` r
# Install CARoT from CRAN:
install.packages("CARoT")

# Or the the development version from GitHub:
# install.packages("remotes")
remotes::install_github("omicsr/CARoT")
```

``` r
library(CARoT)
#>                                                                   
#>   .oooooo.         .o.       ooooooooo.             ooooooooooooo 
#>  d8P'  `Y8b       .888.      `888   `Y88.           8'   888   `8 
#> 888              .8"888.      888   .d88'  .ooooo.       888      
#> 888             .8' `888.     888ooo88P'  d88' `88b      888      
#> 888            .88ooo8888.    888`88b.    888   888      888      
#> `88b    ooo   .8'     `888.   888  `88b.  888   888      888      
#>  `Y8bood8P'  o88o     o8888o o888o  o888o `Y8bod8P'     o888o     
#> 
#> ── Attaching packages ───────────────────────────────────────────────────────────────────── CARoT 0.6.10 ──
#> ✓ ggplot2 3.3.0     ✓ forcats 0.5.0
#> ✓ tibble  2.1.3     ✓ MiSTr   1.0.0
#> ✓ tidyr   1.0.2     ✓ rain    0.3.3
#> ✓ readr   1.3.1     ✓ NACHO   1.0.0
#> ✓ purrr   0.3.3     ✓ dgapaq  0.1.8
#> ✓ dplyr   0.8.5     ✓ dmapaq  0.1.5
#> ✓ stringr 1.4.0
#> Using temporary cache /tmp/RtmpU28hiD/BiocFileCache
#> snapshotDate(): 2019-10-22
#> Using temporary cache /tmp/RtmpU28hiD/BiocFileCache
#> Using temporary cache /tmp/RtmpU28hiD/BiocFileCache
#> ── Conflicts ───────────────────────────────────────────────────────────────────────── carot_conflicts() ──
#> x NACHO::summarize() masks dplyr::summarize()
```

## Overview

*CARoT* (Centralised and Automated Reporting Tools) is an under
development set of Quality-Control reporting tools and some other
functions.

Currently *CARoT* includes the following functions from other packages:

  - [`rain`](https://github.com/mcanouil/rain)
      - `rain::estimate_ethnicity()` allows to format VCF files and
        compute the genomic components (and some figures) for ethnicity
        using [flashpca](https://github.com/gabraham/flashpca) software
      - `rain::pca_report()` allows to compute an analysis report using
        principal component analysis from
        [flashpca](https://github.com/gabraham/flashpca) software.
  - [`MiSTr`](https://github.com/mcanouil/MiSTr)
      - `MiSTr::mist()` allows to test for association between a set of
        SNPS/genes and continuous or binary outcomes by including
        variant characteristic information and using (weighted) score
        statistics.
  - [`dgapaq`](https://github.com/omicsr/dgapaq)
      - `dgapaq::qc_plink()` allows to compute quality-control of
        genotyping array (PLINK format) using a Rmarkdown template.
      - `dgapaq::qc_vcf()` allows to compute post-imputation
        quality-control report using a default Rmarkdown template.
  - [`dmapaq`](https://github.com/omicsr/dmapaq)
      - `dmapaq::ggheatmap()` allows to compute heatmap with dendrogram
        on x-axis and y-axis using
        [ggplot2](https://ggplot2.tidyverse.org/).
      - `dmapaq::read_idats()` allows to efficiently import idats files
        mostly using [minfi](https://bioconductor.org/packages/minfi/)
        functions.
      - `dmapaq::qc_idats()` allows to compute quality-control of
        methylation array from Illumina using a Rmarkdown template.
      - `dmapaq::ComBat.mc()` is a multi-processor wrapper for ComBat
        method (initially in
        [ENmix](https://bioconductor.org/packages/ENmix/)).

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/omicsr/CARoT/issues).  
For questions and other discussion, please contact the package
maintainer.

-----

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md).  
By participating in this project you agree to abide by its terms.
