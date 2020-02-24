#' Compute quality-control of genotyping array (PLINK format) using a rmarkdown template.
#'
#' @param input_directory A `character`. The path to the plink files.
#'     The path should contains the file name without the extension, i.e., without `*.bed`, `*.bim` or `*.fam`.
#' @param output_directory A `character`. The path to the output directory.
#' @param cohort_name A `character`. The name of the studied cohort / population.
#' @param output_file A `character`. The name of the html file produced.
#' @param array A `character`. The array name, e.g., "Illumina Omni2.5".
#' @param callrate_samples A `numeric`. The call rate threshold for samples, under which samples are excluded.
#'     Default is `0.95`.
#' @param callrate_snps A `numeric`. The call rate threshold for probes, under which probes are excluded.
#'     Default is `0.95`.
#' @param heterozygosity_treshold A `numeric`. The heterozygosity threshold for samples
#'    (number of standard deviation from the mean), under/above which samples are excluded.
#'     Default is `4`.
#' @param maf_threshold A `numeric`. The minor allele frequency under which variants are considered "rare".
#'     Default is `0.01`.
#' @param hwe_pvalue A `numeric`. The p-value threshold for Hardy-Weinberg equilibrium test.
#'     Default is `0.0001`.
#' @param includes_relatives A `logical`. Does the data contain related samples?
#'     Default is `FALSE`.
#' @param mendelian_samples A `numeric`. The Mendel error rate threshold above which samples are excluded.
#'     Default is `0.05`.
#' @param mendelian_snp A `numeric`. The Mendel error rate threshold above which variants are excluded.
#'     Default is `0.1`.
#' @param IBD_threshold A `numeric`. The threshold for IBD (identical by descent) above which
#'     samples are characterised as relatives.
#'     Default is `0.2`.
#' @param population A `character`. The ethnicity of the studied population if known, e.g., `"EUR"`.
#'     Default is `NULL`.
#' @param pca_components A `numeric`. The number of principal components to be computed.
#'     Default is `10`.
#' @param pca_threshold A `numeric`. The threshold to define outliers on the principal component analysis,
#'     the as number of standard deviation from the cohort centroid.
#'     Default is `3`.
#' @param check_bim_script A `character`. The PERL script to use to check PLINK files to allow later imputation.
#'     Default is `system.file("perl", "HRC-1000G-check-bim.pl", package = "CARoT")`.
#' @param ref1kg_panel A `character`. The `*.panel` file from 1,000 Genome project.
#'     Default is `NULL`.
#' @param ref1kg_population A `character`. The `*.tsv` file from 1,000 Genome project describing samples and ethnicity.
#'     Default is `NULL`.
#' @param ref1kg_genotypes A `character`. The PLINK files from 1,000 Genome project.
#'     Default is `NULL`.
#' @param ref1kg_legend A `character`. The `*.legend` file from 1,000 Genome project.
#'     Default is `NULL`.
#' @param ref1kg_fasta A `character`. The `*.fasta` file from 1,000 Genome project.
#'     Default is `NULL`.
#' @param bin_path A `list(character)`. A list giving the binary path of `bcftools`, `bgzip`, `gcta` and `plink1.9`.
#' @param title A `character`. The report's title. Default is `paste(array, "Array Quality-Control")`.
#' @param author_name A `character`. The author's name to be printed in the report.
#'     Default is `CARoT`.
#' @param author_affiliation A `character`. The affiliation to be printed in the report.
#'     Default is `NULL`.
#' @param author_email A `character`. The email to be printed in the report.
#'     Default is `NULL`.
#' @param cache A `logical`. Should the R code be cached?
#'     Default is `FALSE`.
#' @param show_code A `logical`. Should the R code be printed in the report?
#'     Default is `FALSE`.
#' @param n_cores A `numeric`. The number of CPUs to use to estimate the ethnicity.
#'     Default is `1`.
#' @param dpi A `numeric`. The value for dpi when plotting the data.
#'     Default is `120`.
#' @param gg_fontsize A `numeric`. Value for the font size. Default is `12`.
#' @param encoding A `character`. The encoding to be used for the html report.
#'     Default is `"UTF-8"`.
#' @param ... Parameters to pass to `rmarkdown::render()`.
#'
#' @return NULL
#' @export
qc_plink <- function(
  input_directory = NULL,
  output_directory = NULL,
  cohort_name = "CARoT",
  output_file = paste0(cohort_name, "_QC.html"),
  array = NULL,
  callrate_samples = 0.95,
  callrate_snps = 0.95,
  heterozygosity_treshold = 4,
  maf_threshold = 0.01,
  hwe_pvalue = 0.0001,
  includes_relatives = FALSE,
  mendelian_samples = 0.05,
  mendelian_snp = 0.1,
  IBD_threshold = 0.2,
  population = NULL,
  pca_components = 10,
  pca_threshold = 3,
  check_bim_script = system.file("perl", "HRC-1000G-check-bim.pl", package = "CARoT"),
  ref1kg_panel = NULL,
  ref1kg_population = NULL,
  ref1kg_genotypes = NULL,
  ref1kg_legend = NULL,
  ref1kg_fasta = NULL,
  bin_path = list(
    bcftools = "/usr/bin/bcftools",
    bgzip = "/usr/bin/bgzip",
    plink = "/usr/bin/plink1.9",
    gcta = "/usr/bin/gcta64"
  ),
  title = paste(array, "Array Quality-Control"),
  author_name = "CARoT",
  author_affiliation = NULL,
  author_email = NULL,
  cache = FALSE,
  show_code = FALSE,
  n_cores = 1,
  dpi = 120,
  gg_fontsize = 12,
  encoding = "UTF-8",
  ...
) {
  message_prefix <- "[CARoT] "

  message(message_prefix, "Quality-Control started ...")
  message(message_prefix, "Note: it can take from one to two hours.")

  file.copy(
    from = system.file("rmarkdown", "templates", "qc_plink", "skeleton.Rmd", package = "umr1283"),
    to = file.path(tempdir(), "qc_plink.Rmd"),
    overwrite = TRUE
  )

  rmarkdown::render(
    input = paste0(tempdir(), "/qc_plink.Rmd"),
    output_file = output_file,
    output_dir = output_directory,
    encoding = encoding,
    params = list(
      input_directory = input_directory,
      output_directory = output_directory,
      cohort_name = cohort_name,
      array = array,
      callrate_samples = callrate_samples,
      callrate_snps = callrate_snps,
      heterozygosity_treshold = heterozygosity_treshold,
      maf_threshold = maf_threshold,
      hwe_pvalue = hwe_pvalue,
      includes_relatives = includes_relatives,
      mendelian_samples = mendelian_samples,
      mendelian_snp = mendelian_snp,
      IBD_threshold = IBD_threshold,
      population = population,
      pca_components = pca_components,
      pca_threshold = pca_threshold,
      check_bim_script = check_bim_script,
      ref1kg_panel = ref1kg_panel,
      ref1kg_population = ref1kg_population,
      ref1kg_genotypes = ref1kg_genotypes,
      ref1kg_legend = ref1kg_legend,
      ref1kg_fasta = ref1kg_fasta,
      bin_path = bin_path,
      title = title,
      author_name = author_name,
      author_affiliation = author_affiliation,
      author_email = author_email,
      cache = cache,
      show_code = show_code,
      n_cores = n_cores,
      dpi = dpi,
      gg_fontsize = gg_fontsize
    ),
    ...
  )

  message(message_prefix, "Quality-Control ended.")

  message(
    paste(
      paste("  ",
        utils::capture.output(
          fs::dir_tree(path = normalizePath(output_directory), recurse = FALSE)
        )
      ),
      collapse = "\n"
    )
  )

  invisible()
}
