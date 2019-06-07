#' qc_plink
#'
#' @param input_directory A `character`.
#' @param output_directory A `character`. The path to the output directory.
#' @param cohort_name A `character`.
#' @param output_file A `character`. The name of the html file produced.
#' @param array A `character`. The array name.
#' @param callrate_samples A `numeric`.
#' @param callrate_snps A `numeric`.
#' @param heterozygosity_treshold A `numeric`.
#' @param maf_threshold A `numeric`.
#' @param hwe_pvalue A `numeric`.
#' @param includes_relatives A `logical`.
#' @param mendelian_samples A `numeric`.
#' @param mendelian_snp A `numeric`.
#' @param IBD_threshold A `numeric`.
#' @param population A `character`.
#' @param pca_components A `numeric`.
#' @param pca_threshold A `numeric`.
#' @param check_bim_script A `character`.
#' @param ref1kg_panel A `character`.
#' @param ref1kg_population A `character`.
#' @param ref1kg_genotypes A `character`.
#' @param ref1kg_legend A `character`.
#' @param ref1kg_fasta A `character`.
#' @param bin_path A `list(character)`.
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
#'     Default is `20`.
#' @param dpi A `numeric`. The value for dpi when plotting the data.
#'     Default is `120`.
#' @param gg_fontsize A `numeric`. Value for the font size. Default is `12`.
#' @param encoding A `character`. The encoding to be used for the html report.
#'     Default is `"UTF-8"`.
#'
#' @return NULL
#' @export
qc_plink <- function(
  input_directory = NULL,
  output_directory = NULL,
  cohort_name = NULL,
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
  n_cores = 22,
  dpi = 120,
  gg_fontsize = 12,
  encoding = "UTF-8"
) {
  file.copy(
    from = system.file("rmarkdown", "qc_plink.Rmd", package = "CARoT"),
    to = paste0(tempdir(), "/qc_plink.Rmd"),
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
    )
  )
}
