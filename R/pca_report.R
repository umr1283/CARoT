#' pca_report
#'
#' @param data A `vector` or `data.frame`. The data on which the PCA has to be performed.
#' @param design A `data.frame`. Additional variables to be used with factorial planes.
#' @param id_var A `character`. The identifier column used to merge the data.
#' @param technical_vars A `vector(character)`. Variables from design to be used with factorial planes.
#' @param n_comp A `numeric`. The number of principal components to be computed.
#' @param fig_n_comp A `numeric`. The number of principal components to be used for figures.
#' @param outliers_component A `logical`. The principal components to be used to outliers detection.
#' @param outliers_threshold A `numeric`. The threshold to define outliers.
#' @param title_level A `numeric`. The markdown title level, i.e., the number of `#` preceding the section.
#'
#' @return A `data.frame`.
#' @export
pca_report <- function(
  data,
  design,
  id_var = "Sample_ID",
  technical_vars,
  n_comp = 5,
  fig_n_comp = n_comp,
  outliers_component = NULL,
  outliers_threshold = 3,
  title_level = 2
) {
  if (!methods::is(design, "data.frame")) {
    design <- as.data.frame(design)
  }
  if (!methods::is(design[, id_var], "character")) {
    design[[id_var]] <- as.character(design[[id_var]])
  }

  keep_technical <- sapply(
    X = design[, technical_vars, drop = FALSE],
    FUN = function(icol) {
      length(unique(icol))>1 & length(unique(icol))!=length(design[, id_var])
    }
  ) %>%
    which() %>%
    names()

  if (length(setdiff(technical_vars, keep_technical))!=0) {
    variables_excluded <- setdiff(technical_vars, keep_technical)
    message(
      "The following variables have been excluded (null variances or confounding with samples): ",
      if (length(variables_excluded) > 1) {
        c(
          paste(variables_excluded[-length(variables_excluded)], collapse = ", "),
          " and ",
          variables_excluded[length(variables_excluded)]
        )
      } else {
        variables_excluded[length(variables_excluded)]
      }
    )
  }

  pca_res <- flashpcaR::flashpca(
    X = t(as.matrix(data)),
    stand = "sd",
    ndim = n_comp
  )

  pca_dfxy <- as.data.frame(pca_res[["projection"]])
  colnames(pca_dfxy) <- paste0("PC", seq_len(ncol(pca_dfxy)))
  pca_dfxy <- dplyr::mutate(.data = pca_dfxy, Sample_ID = as.character(colnames(data)))
  pca_dfxy <- as.data.frame(dplyr::left_join(x = design, y = pca_dfxy, by = id_var))


  cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA inertia contribution {-}\n"))
  p <- tibble::tibble(
    y = (pca_res$values / sum(pca_res$values)),
    x = sprintf("PC%02d", seq_along(pca_res$values))
  ) %>%
    dplyr::mutate(cumsum = cumsum(!!dplyr::sym("y"))) %>%
    ggplot2::ggplot(mapping = ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_bar(stat = "identity", width = 1, colour = "white", fill = "#3B528BFF", na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
    ggplot2::labs(y = "Inertia", x = "PCA Components")
  print(p)
  cat("\n")

  if (length(keep_technical)>0) {
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA factorial planes {- .tabset}\n"))
    for (ivar in keep_technical) {
      cat(paste0("\n", paste(rep("#", title_level + 1), collapse = ""), " ", ivar, " {-}\n"))
      p <- do.call("rbind", apply(t(utils::combn(paste0("PC", seq_len(fig_n_comp)), 2)), 1, function(icoord) {
        tmp <- pca_dfxy[, c(ivar, icoord)]
        tmp[, ivar] <- as.factor(tmp[, ivar])
        colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
        tmp[, "X.PC"] <- icoord[1]
        tmp[, "Y.PC"] <- icoord[2]
        tmp
      })) %>%
        ggplot2::ggplot(mapping = ggplot2::aes_string(x = "X", y = "Y", colour = ivar)) +
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0), na.rm = TRUE) +
        ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = 0), na.rm = TRUE) +
        ggplot2::geom_point(shape = 4, size = 2, na.rm = TRUE) +
        ggplot2::stat_ellipse(type = "norm", na.rm = TRUE) +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!ggplot2::sym("Y.PC")),
          cols = ggplot2::vars(!!ggplot2::sym("X.PC")),
          scales = "free"
        ) +
        ggplot2::guides(colour = ifelse(length(unique(pca_dfxy[, ivar])) <= 12, "legend", "none"))
      print(p)
      cat("\n")
    }

    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA association {-}\n"))
    p <- pca_dfxy %>%
      (function(.data) {
        lapply(seq_len(fig_n_comp), function(i) {
          form <- stats::as.formula(paste0("PC", i, " ~ ", paste(keep_technical, collapse = " + ")))
          stats::lm(form, data = .data) %>%
            stats::anova() %>%
            tibble::rownames_to_column(var = "term") %>%
            dplyr::mutate(PC = i)
        }) %>%
          dplyr::bind_rows()
      }) %>%
      dplyr::filter(!!dplyr::sym("term") != "Residuals") %>%
      dplyr::mutate(term = gsub("factor\\((.*)\\)", "\\1", !!dplyr::sym("term"))) %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = factor(!!ggplot2::sym("PC")),
          y = !!ggplot2::sym("term"),
          fill = !!ggplot2::sym("Pr(>F)")
        )
      ) +
        ggplot2::geom_tile(colour = "white", na.rm = TRUE) +
        ggplot2::geom_text(
          mapping = ggplot2::aes(label = scales::scientific(!!ggplot2::sym("Pr(>F)"), digits = 2)),
          colour = "white",
          size = 3,
          na.rm = TRUE
        ) +
        ggplot2::scale_fill_viridis_c(name = "P-Value", na.value = "grey85", limits = c(0, 0.1)) +
        ggplot2::theme(panel.grid = ggplot2::element_blank()) +
        ggplot2::scale_x_discrete(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::labs(x = "PCA Components", y = NULL)
    print(p)
    cat("\n")
  }


  if (!is.null(outliers_component)) {
    euclid_dist <- dplyr::sym("euclid_dist")
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA Outliers {-}\n"))
    pca_outliers <- pca_dfxy[, paste0("PC", outliers_component), drop = FALSE]^2
    pca_outliers <- sqrt(rowSums(pca_outliers))
    pca_outliers <- tibble::tibble("euclid_dist" = pca_outliers) %>%
      tibble::rownames_to_column(var = id_var) %>%
      dplyr::mutate(
        bad_samples_bool =
          !!euclid_dist <=
            (stats::median(!!euclid_dist) - outliers_threshold * stats::IQR(!!euclid_dist)) |
          !!euclid_dist >=
            (stats::median(!!euclid_dist) + outliers_threshold * stats::IQR(!!euclid_dist)),
        bad_samples = factor(ifelse(!!dplyr::sym("bad_samples_bool"), "BAD", "GOOD"), levels = c("BAD", "GOOD"))
      ) %>%
      tibble::column_to_rownames(var = id_var)

    pca_dfxy <- merge(
      x = pca_dfxy,
      y = pca_outliers,
      by = "row.names"
    ) %>%
      tibble::column_to_rownames(var = "Row.names")

    ivar <- "bad_samples"

    p <- dplyr::bind_rows(apply(t(utils::combn(paste0("PC", seq_len(fig_n_comp)), 2)), 1, function(icoord) {
      tmp <- pca_dfxy[, c(ivar, icoord)]
      tmp[, ivar] <- as.factor(tmp[, ivar])
      colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
      tmp[, "X.PC"] <- icoord[1]
      tmp[, "Y.PC"] <- icoord[2]
      tmp
    })) %>%
      ggplot2::ggplot(mapping = ggplot2::aes_string(x = "X", y = "Y", colour = ivar)) +
      ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0), na.rm = TRUE) +
      ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = 0), na.rm = TRUE) +
      ggplot2::geom_point(shape = 4, size = 2, na.rm = TRUE) +
      ggplot2::stat_ellipse(type = "norm", na.rm = TRUE) +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!ggplot2::sym("Y.PC")),
        cols = ggplot2::vars(!!ggplot2::sym("X.PC")),
        scales = "free"
      )
    print(p)
    cat("\n")

    # pca_dfxy <- pca_dfxy %>%
    #   dplyr::select(-dplyr::starts_with("PC")) %>%
    #   dplyr::filter(bad_samples == "BAD")
  }

  invisible(pca_dfxy)
}
