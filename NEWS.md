# CARoT 0.1.6 (development version)

## Minor improvements and fixes

* In `R/estimate_ethnicity.R`, add roxygen2 documentation for `estimate_ethnicity()`.
* In `R/ggheatmap.R`, add roxygen2 documentation for `ggheatmap()`.
* In `R/pca_report.R`, add roxygen2 documentation for `pca_report()`.
* In `R/qc_idats.R`, add roxygen2 documentation for `qc_idats()`.
* In `/man`, add roxygen2 documentation for `estimate_ethnicity()`, `ggheatmap()`, `pca_report()` and `qc_idats()`.

# CARoT 0.1.5 (development version)

## Minor improvements and fixes

* In `/inst/rmarkdown/qc_idats.Rmd`, fix markdown typos.

# CARoT 0.1.4 (development version)

## Minor improvements and fixes

* In `/inst/rmarkdown/qc_idats.Rmd`, fix __Methods__ section describing what `filter_` parameters do.

# CARoT 0.1.3 (development version)

## Minor improvements and fixes

* In `/R/qc_idats.R`, decrease default dpi value from `300` to `120`.
* In `/inst/rmarkdown/qc_idats.Rmd`, fix the bullet list in gender check section.

# CARoT 0.1.2 (development version)

## Minor improvements and fixes

* In `/R/estimate_ethnicity.R`, add `check_input()` function to properly check inputs using `fs` package.
* In `/inst/rmarkdown/qc_idats.Rmd`, now print call rate parameters in plain text.
* In `/inst/rmarkdown/qc_idats.Rmd` and `/R/qc_idats.R`, `qc_idats()` has a new `cache` parameter.


# CARoT 0.1.1 (development version)

## Minor improvements and fixes

* In `format_sequencing()` (`R/estimate_ethnicity.R`), fix missing arguments from `format_vcf()`.
* In `/inst/rmarkdown/qc_idats.Rmd`, now applies `filter_xy` after gender check.
* In `/inst/rmarkdown/qc_idats.Rmd`, fix `Sample_ID` (*i.e.,* rownames) for gender check.
* In `/R/qc_idats.R`, remove `cat()` messages from `ENmix` functions.


# CARoT 0.1.0 (development version)

## New features

* New `read_idats()` allows to efficiently import idats files mostly using `minfi` functions.
* New `estimate_ethnicity()` allows to compute the genomic component for ethnicity based on VCF files.
* New `pca_report()` allows to compute an analysis report using principal component analysis.  
  The function can be used in Rmarkdown chunk with `results="asis"` to render the report.
* New `ggheatmap()` allows to compute heatmap with dendrogram on x-axis and y-axis.
* New `qc_idats()` allows to compute quality-control of methylation array from Illumina using a default rmarkdown template (`/inst/rmarkdown/qc_idats.Rmd`).
