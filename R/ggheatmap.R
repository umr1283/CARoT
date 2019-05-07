#' ggheatmap
#'
#' @param data description
#' @param dendrogram_columns description
#' @param dendrogram_rows description
#' @param line_size description
#' @param font_size_x description
#' @param font_size_y description
#' @param distance_method description
#' @param cluster_method description
#' @param scale_name description
#' @param scale_revert description
#'
#' @return description
#' @export
ggheatmap <- function(
  data,
  dendrogram_columns = TRUE,
  dendrogram_rows = TRUE,
  line_size = 0.5,
  font_size_x = 11,
  font_size_y = 11,
  distance_method = "euclidean",
  cluster_method = "ward.D2",
  scale_name = "value",
  scale_revert = FALSE
) {
  tidy_data <- data %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "rowname") %>%
    tidyr::gather(key = "variable", value = "value", -"rowname")

  if (dendrogram_rows) {
    dd_row <- stats::as.dendrogram(stats::hclust(stats::dist(data, method = distance_method), method = cluster_method))
    tidy_data[["rowname"]] <- factor(
      x = tidy_data[["rowname"]],
      levels = rownames(data[stats::order.dendrogram(dd_row), ])
    )
  }

  if (dendrogram_columns) {
    dd_col <- stats::as.dendrogram(stats::hclust(stats::dist(t(data), method = distance_method), method = cluster_method))
    tidy_data$variable <- factor(
      x = tidy_data[["variable"]],
      levels = colnames(data[, stats::order.dendrogram(dd_col)])
    )
  }

  heat_plot <- ggplot2::ggplot(
    data = tidy_data,
    mapping = ggplot2::aes_(x = ~variable, y = ~rowname, fill = ~value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(colour = "black"),
      axis.ticks.length = ggplot2::unit(x = 0.5, units = "line"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = font_size_x),
      axis.text.y = ggplot2::element_text(size = font_size_y)
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(position = "right", expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_fill_viridis_c(name = scale_name, direction = as.numeric(!scale_revert))

  final_plot <- heat_plot

  if (dendrogram_rows) {
    dendro_data_row <- ggdendro::dendro_data(dd_row, type = "rectangle")
    dendro_row <- cowplot::axis_canvas(heat_plot, axis = "y", coord_flip = TRUE) +
      ggplot2::geom_segment(
        data = ggdendro::segment(dendro_data_row),
        mapping = ggplot2::aes_(x = ~x, xend = ~xend, y = ~-y, yend = ~-yend),
        size = line_size
      ) +
      ggplot2::coord_flip()
    final_plot <- cowplot::insert_yaxis_grob(heat_plot, dendro_row, grid::unit(0.2, "null"), position = "left")
  }

  if (dendrogram_columns) {
    dendro_data_col <- ggdendro::dendro_data(dd_col, type = "rectangle")
    dendro_col <- cowplot::axis_canvas(heat_plot, axis = "x") +
      ggplot2::geom_segment(
        data = ggdendro::segment(dendro_data_col),
        mapping = ggplot2::aes_(x = ~x, y = ~y, xend = ~xend, yend = ~yend),
        size = line_size
      )
    final_plot <- cowplot::insert_xaxis_grob(final_plot, dendro_col, grid::unit(0.2, "null"), position = "top")
  }

  cowplot::ggdraw(final_plot)
}
