# CARoT 0.1.1

# CARoT 0.1.0.9000 (development version)

## Breaking changes

## New features

* New `read_idats()` allows to efficiently import idats files mostly using `minfi` functions.
* New `estimate_ethnicity()` allows to compute the genomic component for ethnicity based on VCF files.
* New `pca_report()` allows to compute an analysis report using principal component analysis.  
  The function can be used in Rmarkdown chunk with `results="asis"` to render the report.
* New `ggheatmap()` allows to compute heatmap with dendrogram on x-axis and y-axis.
* New `qc_idats()` allows to compute quality-control of methylation array from Illumina using a default rmarkdown template (`/inst/rmarkdown/qc_idats.Rmd`).

## Minor improvements and fixes

* Update title. Removing capital "O" in "RepOrting".
