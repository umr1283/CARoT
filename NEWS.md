# CARoT 0.6.3

# CARoT 0.6.2

## Minor improvements and fixes

* Update `rain` to `v0.3.2`.
* Update `dgapaq` to `v0.1.3`.

# CARoT 0.6.1

## Minor improvements and fixes

* Update packages version in `Imports`.

# CARoT 0.6.0

## New features

* Now attaches the R package `dgapaq`:
    * Remove `R/qc_impute.R`.
    * Remove `R/qc_plinky.R`.
    * Remove `inst/perl/HRC-1000G-check.bim.pl`.
* Now attaches the R package `dmapaq`:
    * Remove `R/qc_idats.R`.
    * Remove `R/read_idats.R`.
    * Remove `R/ggheatmap.R`.
* Remove the R package `umr1283` from the list of attached packages.
* Remove `R/utils-pipe.R`.
* Remove `R/utils-tidy-eval.R`.

# CARoT 0.5.0

## New features

* CARoT is now a meta-package:
    * Now attaches the R package `umr1283`.
    * Now attaches the R package `MiSTr`.
    * Now attaches the R package `rain`.
    * Now attaches the R package `NACHO`.
    * Now attaches the R package `forcats`.
    * Now attaches the R package `stringr`.
    * Now attaches the R package `dplyr`.
    * Now attaches the R package `purrr`.
    * Now attaches the R package `readr`.
    * Now attaches the R package `tidyr`.
    * Now attaches the R package `tibble`.
    * Now attaches the R package `ggplot2`.

## Minor improvements and fixes

* Remove `R/mist.R`, now attaches the R package `MiSTr`.
* Remove `R/estimate_ethnicity.R`, now attaches the R package `rain`.
* Use Rmarkdown template directory for `qc_idats` from `umr1283`.
* Use Rmarkdown template directory for `qc_impute` from `umr1283`.
* Use Rmarkdown template directory for `qc_plink` from `umr1283`.
* In `R/qc_idats.R`, 
    + add rgSet parameters to pass raw rgSet for normalisation and filtering.
    + now return raw rgSet and mSet.


# CARoT 0.4.0

## New features

* New `mist()`, it allows to test for association between a set of SNPS/genes and continuous or binary outcomes by including variant characteristic information and using (weighted) score statistics

## Minor improvements and fixes

* In `R/pca_report.R`, 
    + rewrite core.
    + add check for overall outlier.
    + add example for roxygen.

# CARoT 0.3.3

## Minor improvements and fixes

* In `inst/rmarkdown/qc_idats.Rmd`, 
    + simplify some call to tidyverse functions.
    + add a parameter to control if PCA should be performed.
* In `R/qc_idats.R`, 
    + add a parameter to control if PCA should be performed.
* In `R/estimate_ethnicity.R`, 
    + include super pop and pop from 1K genome.

# CARoT 0.3.2

## Minor improvements and fixes

* In `inst/rmarkdown/qc_idats.Rmd`, 
    + fix and update "methods and parameters" section.
    + remove redundant code, *i.e.*, `as.data.frame`.
    + suppress warnings for gender check, a plot is printed to allow diagnostic.
* In `R/estimate_ethnicity.R`, complete roxygen documentation.

# CARoT 0.3.1

## Minor improvements and fixes

* In `inst/rmarkdown/qc_plink.Rmd`,
    + in `file-management`chunk, each plink steps to prepare files for imputation now 
        generate different files to avoid names conflicts.
* In `R/qc_idats.R`,
    + fix wrong annotation package for Illumina 450k methylation array.
    + change `"MethPipe"` with `"CARoT"` in messages.
* In `inst/rmarkdown/qc_idats.Rmd`,
    + add cord blood panel for cell composition (only 450k).
    + merge methods and parameters sections.

# CARoT 0.3.0

## New features

* New `qc_impute()`, it allows to compute post-imputation quality-control report using a default rmarkdown template (`inst/rmarkdown/qc_impute.Rmd`).

## Minor improvements and fixes

* In `inst/rmarkdown/qc_plink.Rmd`,
    + convert IID to character when reading files.
    + fix missing sheets in Excel exclusion file.
    + fix typos.
* In `R/estimate_ethnicity.R`, 
    + fix a wrong `if` statement when testing the parameters:
        `input_type = "array"` and `splitted_by_chr = FALSE`.
    + fix chromosome pattern recognition to allow VCF files to start with the chromosome id.
    + now check if binary exists at the path prodived in `bin_path` argument.
    + fix when IDs contains "_".
    + fix when chromosome id in VCF includes "chr".
    + add `check_*` functions.
    + export `compute_pca()`.
    + add a `vcf_half_call` parameter to handle half-call when converting VCF to PLINK format.
* In `R/qc_plink.R`,
    + fix roxygen doc for parameter `n_cores` to make it consistent with the current default values.
    + add `cohort_name` parameter.
* In `R/qc_idats.R`,
    + change default parameter value for `cohort_name`.
    + fix roxygen doc for parameter `n_cores` to make it consistent with the current default values.
* In `inst/rmarkdown/qc_plink.Rmd`,
    + replace signle quote with double quotes.
* In `R/pca_report.R`, fix merge issue when the selected column was not `"Sample_ID"` (@mboissel, #2).

# CARoT 0.2.1

## Minor improvements and fixes

* In `inst/rmarkdown/qc_plink.Rmd`,
    + fix YAML header with duplicated parameters.
    + fix duplicated chunk label.
    + fix cross-references.
    + fix variant call rate import.
    + tweak figures to make them more readable.
    + set `include = FALSE` for chunk without figure, table or text output. 
    + add explicit `na.rm = TRUE` in `geom_point()` calls.
    + fix `tidyr::separate()` in `relatedness_03` chunk by setting a more complex separator, i.e., `"_-_"`.
    + replace underscore with dash in chunk names for cross-referencing.
    + fix column names in `exclusion` chunk.
    + make cross-cohort check only if multiple cohorts.
    + the studied population is now plotted in last in PCA.
    + add PCA plot with samples exclusion information.
    + multi-planes PCA plot now highlight outliers in all sub-plot.
    + decrease legend key size in PCA plots.
    + add a exclusion and flag summary in the last table.
    + fix typos.
* In `R/qc_plink.R`,
    + fix params not declared in YAML header of `inst/rmarkdown/qc_plink.Rmd`.
    + complete roxygen documentation.
    + add start/end messages.
    + add prefix for messages.
* In `R/qc_idats.R`,
    + add start/end messages.
    + add prefix for messages.
* In `R/pca_report.R`, 
    + fix a typo in roxygen documentation.
    + add start/end messages.
    + add prefix for messages.
* In `R/estimate_ethnicity.R`, 
    + add prefix for messages.

# CARoT 0.2.0

## New features

* New `qc_plink()`, it allows to compute quality-control of genotyping array (PLINK format) using a default rmarkdown template (`inst/rmarkdown/qc_plink.Rmd`).

## Minor improvements and fixes

* In `DESCRIPTION`, add *RSpectra (<= 0.13-1)* for *flashpcaR* to `Imports`.
* In `DESCRIPTION`, add *data.table* and *qdap* to `Imports`.

# CARoT 0.1.7

## Minor improvements and fixes

* In `inst/rmarkdown/qc_idats.Rmd`, parameters are now described using question "should ... be removed?" 
    instead of "remove ...".
* In `inst/rmarkdown/qc_idats.Rmd`, now includes a section with the parameters used.
* In `R/qc_idats.R`, fix a typo in the `detection_pvalues` parameter's documentation.

# CARoT 0.1.6

## Minor improvements and fixes

* In `R/estimate_ethnicity.R`, add roxygen2 documentation for `estimate_ethnicity()`.
* In `R/ggheatmap.R`, add roxygen2 documentation for `ggheatmap()`.
* In `R/pca_report.R`, add roxygen2 documentation for `pca_report()`.
* In `R/qc_idats.R`, add roxygen2 documentation for `qc_idats()`.
* In `man`, add roxygen2 documentation for `estimate_ethnicity()`, `ggheatmap()`, `pca_report()` and `qc_idats()`.

# CARoT 0.1.5

## Minor improvements and fixes

* In `inst/rmarkdown/qc_idats.Rmd`, fix markdown typos.

# CARoT 0.1.4

## Minor improvements and fixes

* In `inst/rmarkdown/qc_idats.Rmd`, fix __Methods__ section describing what `filter_` parameters do.

# CARoT 0.1.3

## Minor improvements and fixes

* In `R/qc_idats.R`, decrease default dpi value from `300` to `120`.
* In `inst/rmarkdown/qc_idats.Rmd`, fix the bullet list in gender check section.

# CARoT 0.1.2

## Minor improvements and fixes

* In `R/estimate_ethnicity.R`, add `check_input()` function to properly check inputs using `fs` package.
* In `inst/rmarkdown/qc_idats.Rmd`, now print call rate parameters in plain text.
* In `inst/rmarkdown/qc_idats.Rmd` and `R/qc_idats.R`, `qc_idats()` has a new `cache` parameter.


# CARoT 0.1.1

## Minor improvements and fixes

* In `format_sequencing()` (`R/estimate_ethnicity.R`), fix missing arguments from `format_vcf()`.
* In `inst/rmarkdown/qc_idats.Rmd`, now applies `filter_xy` after gender check.
* In `inst/rmarkdown/qc_idats.Rmd`, fix `Sample_ID` (*i.e.,* rownames) for gender check.
* In `R/qc_idats.R`, remove `cat()` messages from `ENmix` functions.


# CARoT 0.1.0

## New features

* New `read_idats()`, it allows to efficiently import idats files mostly using `minfi` functions.
* New `estimate_ethnicity()`, it allows to compute the genomic component for ethnicity based on VCF files.
* New `pca_report()`, it allows to compute an analysis report using principal component analysis.  
  The function can be used in Rmarkdown chunk with `results="asis"` to render the report.
* New `ggheatmap()`, it allows to compute heatmap with dendrogram on x-axis and y-axis.
* New `qc_idats()`, it allows to compute quality-control of methylation array from Illumina using a default rmarkdown template (`inst/rmarkdown/qc_idats.Rmd`).
