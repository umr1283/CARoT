#' qc_idats
#'
#' @param csv_file A `character`.
#' @param data_directory A `character`.
#' @param array A `character`.
#' @param annotation A `character`.
#' @param output_file A `character`.
#' @param output_directory A `character`.
#' @param filter_snps A `logical`.
#' @param filter_non_cpg A `logical`.
#' @param filter_xy A `logical`.
#' @param filter_multihit A `logical`.
#' @param filter_beads A `logical`.
#' @param population A `character`.
#' @param bead_cutoff A `numeric`.
#' @param threshold_detection_pvalues A `numeric`.
#' @param threshold_callrate_samples A `numeric`.
#' @param threshold_callrate_cpgs A `numeric`.
#' @param threshold_gender A `numeric`.
#' @param colname_gender A `character`.
#' @param norm_background A `character`.
#' @param norm_dye A `character`.
#' @param norm_quantile A `character`.
#' @param cell_tissue A `character`.
#' @param pca_vars A `vector(character)`.
#' @param title A `character`.
#' @param author_name A `character`.
#' @param author_affiliation A `character`.
#' @param author_email A `character`.
#' @param show_code A `logical`.
#' @param n_cores A `numeric`.
#' @param dpi A `numeric`.
#' @param gg_fontsize A `numeric`.
#' @param encoding A `character`.
#'
#' @return NULL
#' @export
qc_idats <- function(
  csv_file,
  data_directory,
  array = "EPIC",
  annotation = "ilm10b4.hg19",
  output_file = paste(array, "QC.html"),
  output_directory = ".",
  filter_snps = TRUE,
  filter_non_cpg = TRUE,
  filter_xy = TRUE,
  filter_multihit = TRUE,
  filter_beads = TRUE,
  population = NULL,
  bead_cutoff = 0.05,
  threshold_detection_pvalues = 0.01,
  threshold_callrate_samples = 0.99,
  threshold_callrate_cpgs = 1,
  gender_threshold = -2,
  gender_colname = NULL,
  norm_background = "oob",
  norm_dye = "RELIC",
  norm_quantile = "quantile1",
  cell_tissue = NULL,
  pca_vars = c("Sample_Plate", "Sentrix_ID"),
  title = paste(array, "Array Quality-Control"),
  author_name = "CARoT",
  author_affiliation = NULL,
  author_email = NULL,
  show_code = FALSE,
  n_cores = 20,
  dpi = 300,
  gg_fontsize = 12,
  encoding = "UTF-8"
) {
  file.copy(
    from = system.file("rmarkdown", "qc_idats.Rmd", package = "CARoT"),
    to = paste0(tempdir(), "/qc_idats.Rmd"),
    overwrite = TRUE
  )
  rmarkdown::render(
    input = paste0(tempdir(), "/qc_idats.Rmd"),
    output_file = output_file,
    output_dir = output_directory,
    encoding = encoding,
    params = list(
      title = title,
      author_name = author_name,
      author_affiliation = author_affiliation,
      author_email = author_email,
      output_directory = output_directory,
      show_code = show_code,
      n_cores = n_cores,
      dpi = dpi,
      gg_fontsize = gg_fontsize,
      csv_file = csv_file,
      data_directory = data_directory,
      array = array,
      annotation = annotation,
      filter_snps = filter_snps,
      filter_non_cpg = filter_non_cpg,
      filter_xy = filter_xy,
      filter_multihit = filter_multihit,
      filter_beads = filter_beads,
      population = population,
      bead_cutoff = bead_cutoff,
      threshold_detection_pvalues = detection_pvalues,
      threshold_call_rate_samples = callrate_samples,
      threshold_call_rate_cpgs = callrate_cpgs,
      threshold_gender = gender_threshold,
      colname_gender = gender_colname,
      norm_background = norm_background,
      norm_dye = norm_dye,
      norm_quantile = norm_quantile,
      cell_tissue = cell_tissue,
      pca_vars = pca_vars
    )
  )
}
