# CARoT 0.1.1 (development version)

## Minor improvements and fixes

* In `format_sequencing()` (`R/estimate_ethnicity.R`), fix missing arguments from `format_vcf()`.
* In `/inst/rmarkdown/qc_idats.Rmd`, now applies `filter_xy` after gender check.
* In `/inst/rmarkdown/qc_idats.Rmd`, fix `Sample_ID` (*i.e.,* rownames) for gender check.
* In `/R/c_idats.R`, remove `cat()` messages from `ENmix` functions.
* In `/R/estimate_ethnicity.R`, add `check_input()` function to properly check inputs using `fs` package.

# CARoT 0.1.0

## Breaking changes

* None

## New features

* New `read_idats()` allows to efficiently import idats files mostly using `minfi` functions.
* New `estimate_ethnicity()` allows to compute the genomic component for ethnicity based on VCF files.
* New `pca_report()` allows to compute an analysis report using principal component analysis.  
  The function can be used in Rmarkdown chunk with `results="asis"` to render the report.
* New `ggheatmap()` allows to compute heatmap with dendrogram on x-axis and y-axis.
* New `qc_idats()` allows to compute quality-control of methylation array from Illumina using a default rmarkdown template (`/inst/rmarkdown/qc_idats.Rmd`).

## Minor improvements and fixes

* Update title. Removing capital "O" in "RepOrting".
