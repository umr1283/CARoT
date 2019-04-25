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
  require(flashpcaR)
  require(scales)
  require(tidyverse)
  
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
        paste(variables_excluded[-length(variables_excluded)], collapse = ", "), 
        " and ", 
        variables_excluded[length(variables_excluded)]
    )
  }

  pca_res <- flashpcaR::flashpca(
    X = t(as.matrix(data)),
    stand = "sd",
    ndim = n_comp
  )
  
  pca_dfxy <- pca_res %>%
    `[[`("projection") %>%
    as.data.frame() %>%
    `colnames<-`(paste0("PC", seq_len(ncol(.)))) %>%
    dplyr::mutate(Sample_ID = as.character(colnames(data))) %>% 
    dplyr::left_join(x = design, y = ., by = id_var) %>% 
    as.data.frame()


  cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA inertia contribution {-}\n"))
  p <- tibble::tibble(
    y = (pca_res$values / sum(pca_res$values)), 
    x = sprintf("PC%02d", seq_along(pca_res$values))
  ) %>%
    dplyr::mutate(cumsum = cumsum(y)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat = "identity", width = 1, colour = "white", fill = "#3B528BFF") +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
    ggplot2::labs(y = "Inertia", x = "PCA Components")
  print(p)
  cat("\n")

  if (length(keep_technical)>0) {
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA factorial planes {- .tabset}\n"))
    for (ivar in keep_technical) {
      cat(paste0("\n", paste(rep("#", title_level + 1), collapse = ""), " ", ivar, " {-}\n"))
      p <- do.call("rbind", apply(t(combn(paste0("PC", seq_len(fig_n_comp)), 2)), 1, function(icoord) {
        tmp <- pca_dfxy[, c(ivar, icoord)]
        tmp[, ivar] <- as.factor(tmp[, ivar])
        colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
        tmp[, "X.PC"] <- icoord[1]
        tmp[, "Y.PC"] <- icoord[2]
        tmp
      })) %>%
        ggplot2::ggplot(mapping = ggplot2::aes_string(x = "X", y = "Y", colour = ivar)) +
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0)) +
        ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = 0)) +
        ggplot2::geom_point(shape = 4, size = 2) +
        ggplot2::stat_ellipse(type = "norm") +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::facet_grid(rows = vars(Y.PC), cols = vars(X.PC), scales = "free") +
        ggplot2::guides(colour = ifelse(length(unique(pca_dfxy[, ivar])) <= 12, "legend", "none"))
      print(p)
      cat("\n")
    }

    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA association {-}\n"))
    p <- pca_dfxy %>%
      (function(.data) {
        lapply(seq_len(fig_n_comp), function(i) {
          form <- as.formula(paste0("PC", i, " ~ ", paste(keep_technical, collapse = " + ")))
          lm(form, data = .data) %>% 
            anova() %>% 
            tibble::rownames_to_column(var = "term") %>% 
            dplyr::mutate(PC = i)
        }) %>%
          dplyr::bind_rows()
      }) %>%
      dplyr::filter(term != "Residuals") %>%
      dplyr::mutate(term = gsub("factor\\((.*)\\)", "\\1", term)) %>%
      ggplot2::ggplot(mapping = ggplot2::aes(x = factor(PC), y = term, fill = `Pr(>F)`)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(
        mapping = ggplot2::aes(label = scales::scientific(`Pr(>F)`, digits = 2)), 
        colour = "white", 
        size = 3
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
    cat(paste0("\n", paste(rep("#", title_level), collapse = ""), " PCA Outliers {-}\n"))
    pca_outliers <- pca_dfxy[, paste0("PC", outliers_component), drop = FALSE] %>%
      `^`(2) %>%
      rowSums() %>%
      sqrt() %>%
      tibble::tibble(EuclideanDistance = .) %>%
      tibble::rownames_to_column(var = id_var) %>%
      dplyr::mutate(
        BadSamplesLogical = 
          EuclideanDistance <= 
            (median(EuclideanDistance) - outliers_threshold * IQR(EuclideanDistance)) |
          EuclideanDistance >= 
            (median(EuclideanDistance) + outliers_threshold * IQR(EuclideanDistance)),
        BadSamples = factor(ifelse(BadSamplesLogical, "BAD", "GOOD"), levels = c("BAD", "GOOD"))
      ) %>%
      tibble::column_to_rownames(var = id_var)
  
    pca_dfxy <- merge(
      x = pca_dfxy,
      y = pca_outliers,
      by = "row.names"
    ) %>%
      tibble::column_to_rownames(var = "Row.names")
    
    ivar <- "BadSamples"
    
    p <- dplyr::bind_rows(apply(t(combn(paste0("PC", seq_len(fig_n_comp)), 2)), 1, function(icoord) {
      tmp <- pca_dfxy[, c(ivar, icoord)]
      tmp[, ivar] <- as.factor(tmp[, ivar])
      colnames(tmp)[-seq_len(ncol(tmp) - 2)] <- c("X", "Y")
      tmp[, "X.PC"] <- icoord[1]
      tmp[, "Y.PC"] <- icoord[2]
      tmp
    })) %>%
      ggplot2::ggplot(mapping = ggplot2::aes_string(x = "X", y = "Y", colour = ivar)) +
      ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0)) +
      ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = 0)) +
      ggplot2::geom_point(shape = 4, size = 2) +
      ggplot2::stat_ellipse(type = "norm") +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::facet_grid(rows = vars(Y.PC), cols = vars(X.PC), scales = "free")
    print(p)
    cat("\n")
    
    # pca_dfxy <- pca_dfxy %>%
    #   dplyr::select(-dplyr::starts_with("PC")) %>%
    #   dplyr::filter(BadSamples == "BAD")
  }
  
  invisible(pca_dfxy)
}
