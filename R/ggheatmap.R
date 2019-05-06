#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param segments PARAM_DESCRIPTION
#' @param labels PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.gapdata
#' @export
as.gapdata <- function(d, segments, labels, ...) {
  x <- list(dendrogram = d, segments = segments, labels = labels)
  class(x) <- "gapdata"
  x
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname assign_branch_positions
#' @export
assign_branch_positions <- function(d, verbose = FALSE, ...) {
  left <- d[[1]]
  right <- d[[2]]
  if (is.leaf(left) && is.leaf(right)) {
    # set attribute for d
    xleft <- attr(left, "xpos")
    xright <- attr(right, "xpos")
    attr(d, "xleft") <- xleft
    attr(d, "xright") <- xright
    attr(d, "xmid") <- (xleft + xright) / 2
    attr(d, "midpoint") <- (xleft + xright) / 2
  } else if (!is.leaf(left) && is.leaf(right)) {
    # set the lower branch posistion first
    left <- assign_branch_positions(left, verbose = verbose)
    d[[1]] <- left
    # get left middle point
    xleft <- attr(left, "xmid")
    xright <- attr(right, "xpos")
    attr(d, "xleft") <- xleft
    attr(d, "xright") <- xright
    attr(d, "xmid") <- (xleft + xright) / 2
    attr(d, "midpoint") <- attr(d, "xmid") - attr(left, "xleft")
  } else if (is.leaf(left) && !is.leaf(right)) {
    # set the lower branch posistion first
    right <- assign_branch_positions(right, verbose = verbose)
    d[[2]] <- right
    xleft <- attr(left, "xpos")
    # get right middle point
    xright <- attr(right, "xmid")
    attr(d, "xleft") <- xleft
    attr(d, "xright") <- xright
    attr(d, "xmid") <- (xleft + xright) / 2
    attr(d, "midpoint") <- attr(d, "xmid") - attr(left, "xleft")
  } else {
    # both subtree
    left <- assign_branch_positions(left, verbose = verbose)
    d[[1]] <- left
    right <- assign_branch_positions(right, verbose = verbose)
    d[[2]] <- right

    xleft <- attr(left, "xmid")
    xright <- attr(right, "xmid")
    attr(d, "xleft") <- xleft
    attr(d, "xright") <- xright
    attr(d, "xmid") <- (xleft + xright) / 2

    most_left <- get_most_left_leaf(left)
    attr(d, "midpoint") <- attr(d, "xmid") - attr(most_left, "xpos") # most left
  }
  d
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param runningX PARAM_DESCRIPTION, Default: 1
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname assign_positions
#' @export
assign_positions <- function(d, runningX = 1, verbose = FALSE, ...) {
  left <- d[[1]]
  right <- d[[2]]
  if (is.leaf(left) && is.leaf(right)) {
    if (verbose) print("leaf_leaf")
    # both leaves
    attr(left, "xpos") <- runningX
    if (verbose) print(paste0("    ", "debug: set runningX for ", attr(left, "label"), " is ", runningX))
    runningX <- runningX + attr(left, "right_gap") + 1
    attr(right, "xpos") <- runningX
    if (verbose) print(paste0("    ", "debug: set runningX for ", attr(right, "label"), " is ", runningX))
  } else if (!is.leaf(left) && is.leaf(right)) {
    if (verbose) print("sub_leaf")
    # set the positions on the left first
    left <- assign_positions(left, runningX, verbose = verbose)
    # get the right most leaf of the subtree
    right_most <- get_most_right_leaf(left)

    runningX <- attr(right_most, "xpos") + attr(right_most, "right_gap") + 1
    if (verbose) print(paste0("sub_leaf: runningX for ", attr(right_most, "label"), " is ", runningX, ", xpos is ", attr(right_most, "xpos"), ", right_gap is ", attr(right_most, "right_gap")))
    attr(right, "xpos") <- runningX
    if (verbose) print(paste0("    ", "debug: set runningX for ", attr(right, "label"), " is ", runningX))
  } else if (is.leaf(left) && !is.leaf(right)) {
    if (verbose) print("leaf_sub")
    # set the position of the left leaf
    attr(left, "xpos") <- runningX
    if (verbose) print(paste0("    ", "debug: set runningX for ", attr(left, "label"), " is ", runningX))
    runningX <- runningX + attr(left, "right_gap") + 1
    # set the position on the right subtree
    right <- assign_positions(right, runningX, verbose = verbose)
  } else {
    # both subtree
    if (verbose) print("sub_sub")
    # set the positions on the left first
    left <- assign_positions(left, runningX, verbose = verbose)
    # get the right most leaf of the subtree
    right_most <- get_most_right_leaf(left)
    runningX <- attr(right_most, "xpos") + attr(right_most, "right_gap") + 1
    right <- assign_positions(right, runningX, verbose = verbose)
  }
  d[[1]] <- left
  d[[2]] <- right
  d
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param sum PARAM_DESCRIPTION
#' @param gap_total PARAM_DESCRIPTION
#' @param mode PARAM_DESCRIPTION, Default: c("quantitative", "threshold")
#' @param mapping PARAM_DESCRIPTION, Default: c("exponential", "linear")
#' @param scale PARAM_DESCRIPTION, Default: 0.2
#' @param max_height PARAM_DESCRIPTION, Default: 0
#' @param threshold PARAM_DESCRIPTION, Default: 2
#' @param gap_size PARAM_DESCRIPTION, Default: 0
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname calculate_gap
#' @export
calculate_gap <- function(d, sum, gap_total, mode = c("quantitative", "threshold"), mapping = c("exponential", "linear"), scale = 0.2, max_height = 0, threshold = 2, gap_size = 0, verbose = FALSE, ...) {
  a <- attributes(d) # attributes
  left <- d[[1]]
  right <- d[[2]]
  # distance of this branch
  h <- a$height
  # map this distance to allocated space
  mode <- match.arg(mode)
  gap <- 0
  if (mode == "quantitative") {
    mapping <- match.arg(mapping)
    if (mapping == "linear") {
      # linear mapping
      gap <- ggmap(h, 0, sum, 0, gap_total)
    } else if (mapping == "exponential") {
      # exponential mapping
      h.exp <- map.exp(h, 0, max_height, 0, 1, scale = scale)
      gap <- ggmap(h.exp, 0, sum, 0, gap_total)
    }
  } else if (mode == "threshold") {
    if (h > threshold) {
      gap <- gap_size
    }
  }

  # print(paste0("verbose: h=", format_number(h), " gap_total=", format_number(gap_total), " sum=", format_number(sum), " gap=", gap))

  if (is.leaf(left) && is.leaf(right)) {
    # save the gap on the left
    attr(left, "right_gap") <- gap
    if (verbose) print(paste0("gap of ", attr(left, "label"), " is ", format_number(gap)))
  } else if (!is.leaf(left) && is.leaf(right)) {
    # find the most right leaf
    most_right <- get_most_right_leaf(left)
    # assign position for right
    attr(most_right, "right_gap") <- gap
    # assign the value
    if (verbose) print(paste0("gap of ", attr(most_right, "label"), " is ", format_number(gap), " sub_leaf"))
    left <- set_most_right_leaf(left, most_right)
    # go deeper on left next
    left <- calculate_gap(d = left, sum = sum, gap_total = gap_total, mode = mode, mapping = mapping, scale = scale, max_height = max_height, threshold = threshold, gap_size = gap_size, verbose = verbose)
  } else if (is.leaf(left) && !is.leaf(right)) {
    # save the gap on the left
    attr(left, "right_gap") <- gap
    if (verbose) print(paste0("gap of ", attr(left, "label"), " is ", format_number(gap)))
    # go into the next layer
    right <- calculate_gap(d = right, sum = sum, gap_total = gap_total, mode = mode, mapping = mapping, scale = scale, max_height = max_height, threshold = threshold, gap_size = gap_size, verbose = verbose)
  } else {
    # find the most right leaf
    most_right <- get_most_right_leaf(left)
    attr(most_right, "right_gap") <- gap
    if (verbose) print(paste0("gap of ", attr(most_right, "label"), " is ", format_number(gap), " sub_sub"))
    # assign the value
    left <- set_most_right_leaf(left, most_right)
    # go into the next layer
    left <- calculate_gap(d = left, sum = sum, gap_total = gap_total, mode = mode, mapping = mapping, scale = scale, max_height = max_height, threshold = threshold, gap_size = gap_size, verbose = verbose)
    right <- calculate_gap(d = right, sum = sum, gap_total = gap_total, mode = mode, mapping = mapping, scale = scale, max_height = max_height, threshold = threshold, gap_size = gap_size, verbose = verbose)
  }
  d[[1]] <- left
  d[[2]] <- right
  d
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION, Default: d
#' @param count PARAM_DESCRIPTION, Default: 0
#' @param threshold PARAM_DESCRIPTION, Default: threshold
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname count_gap
#' @export
count_gap <- function(d = d, count = 0, threshold = threshold) {
  a <- attributes(d) # attributes
  height <- a$height
  left <- d[[1]]
  right <- d[[2]]
  gapped <- ifelse((height > threshold), TRUE, FALSE)
  if (gapped) count <- count + 1 # increment
  if (is.leaf(left) && is.leaf(right)) {
  } else if (!is.leaf(left) && is.leaf(right)) {
    if (gapped) {
      count <- count_gap(left, count = count, threshold = threshold)
    }
  } else if (is.leaf(left) && !is.leaf(right)) {
    if (gapped) {
      count <- count_gap(right, count = count, threshold = threshold)
    }
  } else {
    if (gapped) {
      count <- count_gap(left, count = count, threshold = threshold)
      count <- count_gap(right, count = count, threshold = threshold)
    }
  }
  return(count)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param segments_df PARAM_DESCRIPTION, Default: NULL
#' @param labels_df PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname extract_list
#' @export
extract_list <- function(d, type, segments_df = NULL, labels_df = NULL, ...) {
  inner <- !is.leaf(d) # check if it is subtree
  yTop <- attr(d, "height") # height of subtree
  xMid <- attr(d, "xmid")

  if (is.leaf(d)) { # singleton cluster
    Y <- yTop
    X <- attr(d, "xpos")
    nodeText <- as.character(attr(d, "label"))
    labels_df <- rbind(labels_df, data.frame(x = X, y = 0, text = nodeText))
  } else if (inner) { # subtree
    for (k in seq_along(d)) {
      child <- d[[k]]
      yBot <- attr(child, "height")
      if (is.null(yBot)) {
        yBot <- 0
      }
      xBot <- 0
      if (is.leaf(child)) {
        xBot <- attr(child, "xpos")
      } else {
        xBot <- attr(child, "xmid")
        if (is.null(attr(child, "xmid"))) print("xmid is nulll")
      }
      if (type == "triangle") {
        # adding lines
        segments_df <- rbind(segments_df, get_segment_df(xMid, yTop, xBot, yBot))
      } else {
        segments_df <- rbind(segments_df, get_segment_df(xMid, yTop, xBot, yTop))
        segments_df <- rbind(segments_df, get_segment_df(xBot, yTop, xBot, yBot))
      }
      # call recursively
      temp_list <- extract_list(d = child, type, segments_df, labels_df)
      segments_df <- temp_list$segments
      labels_df <- temp_list$labels
    }
  }
  l <- list(segments = segments_df, labels = labels_df)
  return(l)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param mode PARAM_DESCRIPTION, Default: c("quantitative", "threshold")
#' @param mapping PARAM_DESCRIPTION, Default: c("exponential", "linear")
#' @param ratio PARAM_DESCRIPTION, Default: 0.2
#' @param scale PARAM_DESCRIPTION, Default: 0.5
#' @param threshold PARAM_DESCRIPTION, Default: 0
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gap_data
#' @export
gap_data <- function(d, mode = c("quantitative", "threshold"), mapping = c("exponential", "linear"), ratio = 0.2, scale = 0.5, threshold = 0, verbose = FALSE, ...) {
  # arguments
  mode <- match.arg(mode)
  # number of nodes
  N <- attr(d, "members")
  if (verbose) print(paste0("total number of nodes = ", N))
  # allocate gap space (default 20%)
  gap_total <- ratio * N
  if (verbose) print(paste0("total length of gap = ", gap_total))

  # annotate gap to dendrogram
  if (mode == "quantitative") {
    mapping <- match.arg(mapping)
    max_height <- attr(d, "height")
    # calculate the sum of distance
    sum <- sum_distance(d, sum = 0, mapping = mapping, scale = scale, max_height = max_height)
    # recursively calculate gap
    if (verbose) print("calculate_gap() -----------")
    d <- calculate_gap(d = d, sum = sum, gap_total = gap_total, mode = mode, mapping = mapping, scale = scale, max_height = max_height, verbose = verbose)
  } else if (mode == "threshold") {
    # count total number of branches above the threshold
    count <- count_gap(d = d, count = 0, threshold = threshold)
    # calculate gap_size
    gap_size <- gap_total / count
    # recursively calculate gap
    if (verbose) print("calculate_gap() -----------")
    d <- calculate_gap(d = d, mode = mode, gap_size = gap_size, threshold = threshold, verbose = verbose)
  }

  # re-evaluate the positions for each leaves
  if (verbose) print("assign_positions() -----------")
  d <- assign_positions(d, runningX = 1, verbose = verbose)

  # re-evaluate the branch positions
  d <- assign_branch_positions(d)

  # extract a list
  l <- extract_list(d, type = "rectangle")
  # add column names
  names(l$segments) <- c("x", "y", "xend", "yend")
  names(l$labels) <- c("x", "y", "label")

  # compose a gapdata class object
  output <- as.gapdata(d = d, segments = l$segments, labels = l$labels)

  output # return
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param value PARAM_DESCRIPTION
#' @param start1 PARAM_DESCRIPTION
#' @param stop1 PARAM_DESCRIPTION
#' @param start2 PARAM_DESCRIPTION
#' @param stop2 PARAM_DESCRIPTION
#' @param scale PARAM_DESCRIPTION, Default: 0.5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname map.exp
#' @export
map.exp <- function(value, start1, stop1, start2, stop2, scale = 0.5) {
  f <- (value - start1) / (stop1 - start1)
  flog <- f^(1 / scale)
  return(flog * (stop2 - start2))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param value PARAM_DESCRIPTION
#' @param start1 PARAM_DESCRIPTION
#' @param stop1 PARAM_DESCRIPTION
#' @param start2 PARAM_DESCRIPTION
#' @param stop2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname map
#' @export
ggmap <- function(value, start1, stop1, start2, stop2) {
  return(start2 + (stop2 - start2) * ((value - start1) / (stop1 - start1)))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param d2 PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname set_most_right_leaf
#' @export
set_most_right_leaf <- function(d, d2, ...) {
  right <- d[[2]]
  if (is.leaf(right)) {
    # set the value
    d[[2]] <- d2
  } else {
    # go deeper
    d[[2]] <- set_most_right_leaf(right, d2)
  }
  d # return
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_most_left_leaf
#' @export
get_most_left_leaf <- function(d) {
  left <- d[[1]]
  if (!is.leaf(left)) {
    left <- get_most_left_leaf(left)
  }
  left
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_most_right_leaf
#' @export
get_most_right_leaf <- function(d) {
  right <- d[[2]]
  if (!is.leaf(right)) {
    right <- get_most_right_leaf(right)
  }
  right
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param sum PARAM_DESCRIPTION, Default: 0
#' @param mapping PARAM_DESCRIPTION, Default: c("exponential", "linear")
#' @param scale PARAM_DESCRIPTION, Default: 0
#' @param max_height PARAM_DESCRIPTION, Default: 0
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sum_distance
#' @export
sum_distance <- function(d, sum = 0, mapping = c("exponential", "linear"), scale = 0, max_height = 0, ...) {
  a <- attributes(d) # attributes
  height <- a$height
  mapping <- match.arg(mapping)
  if (mapping == "exponential") {
    height <- map.exp(height, 0, max_height, 0, 1, scale = scale)
  }
  left <- d[[1]]
  right <- d[[2]]
  if (is.leaf(left) && is.leaf(right)) {
    sum <- sum + height
  } else if (!is.leaf(left) && is.leaf(right)) {
    sum <- sum + height
    sum <- sum_distance(left, sum, mapping = mapping, scale = scale, max_height = max_height)
  } else if (is.leaf(left) && !is.leaf(right)) {
    sum <- sum + height
    sum <- sum_distance(right, sum, mapping = mapping, scale = scale, max_height = max_height)
  } else {
    sum <- sum + height
    sum <- sum_distance(left, sum, mapping = mapping, scale = scale, max_height = max_height)
    sum <- sum_distance(right, sum, mapping = mapping, scale = scale, max_height = max_height)
  }
  return(sum)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x0 PARAM_DESCRIPTION
#' @param y0 PARAM_DESCRIPTION
#' @param x1 PARAM_DESCRIPTION
#' @param y1 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_segment_df
#' @export
get_segment_df <- function(x0, y0, x1, y1) {
  data.frame(x0, y0, x1, y1)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param gap.ratio PARAM_DESCRIPTION, Default: 0.05
#' @param grid.h.ratio PARAM_DESCRIPTION, Default: c(0.15, 0.6, 0.25)
#' @param grid.v.ratio PARAM_DESCRIPTION, Default: rev(grid.h.ratio)
#' @param legend.title PARAM_DESCRIPTION, Default: NULL
#' @param legend.position PARAM_DESCRIPTION, Default: c(which.max(c(grid.h.ratio[1], 0, grid.h.ratio[3])), which.max(c(grid.v.ratio[1],
#'    0, grid.v.ratio[3])))
#' @param label.h.size PARAM_DESCRIPTION, Default: 3
#' @param label.v.size PARAM_DESCRIPTION, Default: label.h.size
#' @param label.h.colour PARAM_DESCRIPTION, Default: 'black'
#' @param label.v.colour PARAM_DESCRIPTION, Default: 'black'
#' @param segments.colour PARAM_DESCRIPTION, Default: FALSE
#' @param trans.colour PARAM_DESCRIPTION, Default: NULL
#' @param print PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[reshape2]{melt}}
#' @rdname ggheatmap
#' @export
#' @importFrom reshape2 melt
ggheatmap <- function(
  data,
  gap.ratio = 0.05,
  grid.h.ratio = c(0.15, 0.60, 0.25),
  grid.v.ratio = rev(grid.h.ratio),
  legend.title = NULL,
  legend.position = c(which.max(c(grid.h.ratio[1], 0, grid.h.ratio[3])), which.max(c(grid.v.ratio[1], 0, grid.v.ratio[3]))),
  label.h.size = 3,
  label.v.size = label.h.size,
  label.h.colour = "black",
  label.v.colour = "black",
  segments.colour = FALSE,
  trans.colour = NULL,
  print = TRUE
) {
  set_colour <- function() {
    ggplot2::theme_get()$plot.background$colour %>%
      grDevices::col2rgb() %>%
      range() %>%
      sum() %>%
      (function(.x) {
        if ((.x * 100 * 0.5 / 255) >= 50) {
          "black"
        } else {
          "white"
        }
      })()
  }
  default_colour <- set_colour()

  if (label.h.colour %in% c("white", "black") & label.h.colour != default_colour) {
    label.h.colour <- default_colour
  }

  if (label.v.colour %in% c("white", "black") & label.v.colour != default_colour) {
    label.v.colour <- default_colour
  }

  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  if (is.null(rownames(data))) {
    rownames(data) <- seq_len(nrow(data))
  }
  if (is.null(colnames(data))) {
    colnames(data) <- seq_len(ncol(data))
  }

  row_hc <- hclust(dist(data), "ward.D")
  col_hc <- hclust(dist(t(data)), "ward.D")

  d_row <- rev(as.dendrogram(row_hc))
  d_col <- as.dendrogram(col_hc)

  if (length(label.h.colour) == 1) {
    label.h.colour <- rep(label.h.colour, length(row_hc$order))
  }
  label.h.colour <- as.character(label.h.colour[rev(row_hc$order)])

  if (length(label.v.colour) == 1) {
    label.v.colour <- rep(label.v.colour, length(col_hc$order))
  }
  label.v.colour <- as.character(label.v.colour[col_hc$order])

  row_data <- gap_data(
    d = d_row,
    mode = "quantitative",
    mapping = "exponential",
    ratio = gap.ratio,
    threshold = 0,
    verbose = FALSE,
    scale = 0.5
  )
  row_data$labels$lab <- row_data$labels$label
  row_data$labels$colour <- label.h.colour

  if (segments.colour) {
    row_tmpdta <- row_data$segments
    row_tmpdta <- row_tmpdta[order(row_tmpdta[, "x"], row_tmpdta[, "y"]), ]
    row_tmpdta[, "group"] <- NA
    row_tmpdta[row_tmpdta[, "x"] == row_tmpdta[, "xend"] & row_tmpdta[, "yend"] == 0, "group"] <- row_data$labels[, "colour"]
    # row_tmpdta <- row_tmpdta %>%
    # group_by(y) %>%
    # mutate(., colour = ifelse(length(unique(na.exclude(group)))==1, unique(group), "default")) %>%
    # data.frame
    # row_tmpdta[, "colour"] <- ifelse(is.na(row_tmpdta[, "group"]), row_tmpdta[, "colour"], row_tmpdta[, "group"])
    # row_tmpdta[row_tmpdta[, "colour"]%in%"default", "colour"] <- NA
    row_tmpdta[, "colour"] <- row_tmpdta[, "group"]
    row_data$segments <- row_tmpdta
  }

  col_data <- gap_data(
    d = d_col,
    mode = "quantitative",
    mapping = "exponential",
    ratio = gap.ratio,
    threshold = 0,
    verbose = FALSE,
    scale = 0.5
  )
  col_data$labels$lab <- col_data$labels$label
  col_data$labels$colour <- label.v.colour

  if (segments.colour) {
    col_tmpdta <- col_data$segments
    col_tmpdta <- col_tmpdta[order(col_tmpdta[, "x"], col_tmpdta[, "y"]), ]
    col_tmpdta[, "group"] <- NA
    col_tmpdta[col_tmpdta[, "x"] == col_tmpdta[, "xend"] & col_tmpdta[, "yend"] == 0, "group"] <- col_data$labels[, "colour"]
    # col_tmpdta <- col_tmpdta %>%
    # group_by(y) %>%
    # mutate(., colour = ifelse(length(unique(na.exclude(group)))==1, unique(group), "default")) %>%
    # data.frame
    # col_tmpdta[, "colour"] <- ifelse(is.na(col_tmpdta[, "group"]), col_tmpdta[, "colour"], col_tmpdta[, "group"])
    # col_tmpdta[col_tmpdta[, "colour"]%in%"default", "colour"] <- NA
    col_tmpdta[, "colour"] <- col_tmpdta[, "group"]
    col_data$segments <- col_tmpdta
  }

  left_item <- ggplot(data = row_data$label, aes_string(x = "y", y = "x", label = "label", colour = "colour")) +
    scale_y_continuous(expand = c(0, 0), limits = c(min(row_data$labels$x) - 0.5, max(row_data$labels$x) + 0.5)) +
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 0)) +
    ifelse(length(unique(label.h.colour)) == 1,
      list(geom_text(hjust = 1, size = label.h.size, colour = default_colour)),
      list(geom_text(hjust = 1, size = label.h.size))
    ) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      panel.spacing = unit(c(0, 0, 0, 0), "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "none"
    ) +
    labs(x = NULL, y = NULL) +
    scale_colour_viridis_d()

  right_item <- ggplot() +
    ifelse(length(unique(label.h.colour)) == 1 | !segments.colour,
      list(geom_segment(data = row_data$segments, aes_string(x = "x", y = "y", xend = "xend", yend = "yend"), colour = default_colour)),
      list(geom_segment(data = row_data$segments, aes_string(x = "x", y = "y", xend = "xend", yend = "yend", colour = "colour")))
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(min(row_data$labels$x) - 0.5, max(row_data$labels$x) + 0.5)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      panel.spacing = unit(c(0, 0, 0, 0), "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "none"
    ) +
    labs(x = NULL, y = NULL) +
    scale_colour_viridis_d(na.value = default_colour)


  top_item <- ggplot() +
    ifelse(length(unique(label.v.colour)) == 1 | !segments.colour,
      list(geom_segment(data = col_data$segments, aes_string(x = "x", y = "y", xend = "xend", yend = "yend"), colour = default_colour)),
      list(geom_segment(data = col_data$segments, aes_string(x = "x", y = "y", xend = "xend", yend = "yend", colour = "colour")))
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(min(col_data$labels$x) - 0.5, max(col_data$labels$x) + 0.5)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      panel.spacing = unit(c(0, 0, 0, 0), "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "none"
    ) +
    labs(x = NULL, y = NULL) +
    scale_colour_viridis_d(na.value = default_colour)

  bottom_item <- ggplot(data = col_data$label, aes_string(x = "x", y = "y", label = "label", colour = "colour")) +
    scale_x_continuous(expand = c(0, 0), limits = c(min(col_data$labels$x) - 0.5, max(col_data$labels$x) + 0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 0)) +
    ifelse(length(unique(label.v.colour)) == 1,
      list(geom_text(angle = 90, hjust = 1, size = label.v.size, colour = default_colour)),
      list(geom_text(angle = 90, hjust = 1, size = label.v.size))
    ) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      panel.spacing = unit(c(0, 0, 0, 0), "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "none"
    ) +
    labs(x = NULL, y = NULL) +
    scale_colour_viridis_d()

  row_data$labels$label <- row_data$labels$lab
  col_data$labels$label <- col_data$labels$lab

  r_order <- match(row_data$labels$label, rownames(data))
  c_order <- match(col_data$labels$label, colnames(data))
  M <- data[r_order, c_order]
  rownames(M) <- row_data$labels$x
  colnames(M) <- col_data$labels$x
  M <- reshape2::melt(M)
  colnames(M) <- c("y", "x", "value")
  center_item <- ggplot(data = M, aes_string(x = "x", y = "y")) +
    theme_void() +
    geom_tile(aes_string(fill = "value")) +
    scale_fill_viridis_c(name = legend.title) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "none"
    )
  
  if (is.null(trans.colour)) {
    center_item <- center_item + 
      scale_fill_viridis_c(name = legend.title)
  } else {
    center_item <- center_item + 
      scale_fill_viridis_c(name = legend.title, trans = trans.colour)
  }


  hm.leg <- center_item +
    theme(
      legend.position = c(0.50, 0.50),
      legend.text = element_text(colour = default_colour),
      legend.title = element_text(colour = default_colour)
    ) +
    guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title.hjust = 0.5))
  hm.leg <- ggplot_gtable(ggplot_build(hm.leg))
  hm.legend <- hm.leg$grobs[[which(sapply(hm.leg$grobs, function(x) x$name) == "guide-box")]]


  left_item <- left_item + theme(plot.margin = unit(c(0, 0.2, 0, 0), "lines"))
  top_item <- top_item + theme(plot.margin = unit(c(2, 0, 0, 0), "lines"))
  right_item <- right_item + theme(plot.margin = unit(c(0, 2, 0, 0), "lines"))
  bottom_item <- bottom_item + theme(plot.margin = unit(c(0.2, 0, 0, 0), "lines"))
  center_item <- center_item + theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))


  top.height <- grid.v.ratio[1]
  center.height <- grid.v.ratio[2]
  bottom.height <- grid.v.ratio[3]
  left.width <- grid.h.ratio[1]
  center.width <- grid.h.ratio[2]
  right.width <- grid.h.ratio[3]
  row.n <- 3
  col.n <- 3


  output <- list(left_item = left_item, top_item = top_item, right_item = right_item, bottom_item = bottom_item, center_item = center_item, hm.legend = hm.legend)

  if (print) {
    ggheatmap.show(
      data = output, 
      grid.h.ratio = grid.h.ratio, 
      grid.v.ratio = grid.v.ratio, 
      legend.position = legend.position
    )
  }

  return(invisible(output))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param grid.h.ratio PARAM_DESCRIPTION, Default: c(0.15, 0.6, 0.25)
#' @param grid.v.ratio PARAM_DESCRIPTION, Default: rev(grid.h.ratio)
#' @param legend.position PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggheatmap.show
#' @export
ggheatmap.show <- function(
  data, 
  grid.h.ratio = c(0.15, 0.60, 0.25), 
  grid.v.ratio = rev(grid.h.ratio), 
  legend.position = NULL
) {
  ggdraw <- function(plot = NULL, xlim = c(0, 1), ylim = c(0, 1)) {
    set_colour <- function() {
      ggplot2::theme_get()$plot.background$colour %>%
        grDevices::col2rgb() %>%
        range() %>%
        sum() %>%
        (function(.x) {
          if ((.x * 100 * 0.5 / 255) >= 50) {
            "black"
          } else {
            "white"
          }
        })()
    }
    default_colour <- set_colour()
    base_colour <- ggplot2::theme_get()$plot.background$colour
    
    theme_nothing <- function(base_size = 14, base_family = "") {
      theme_void(base_size = base_size, base_family = base_family) %+replace%
        theme(
          line = element_blank(), 
          rect = element_blank(),
          text = element_text(
            family = base_family, 
            face = "plain",
            colour = default_colour, 
            size = base_size, 
            lineheight = 0.9,
            hjust = 0.5, 
            vjust = 0.5, 
            angle = 0, 
            margin = margin(),
            debug = FALSE
          ), 
          axis.line = element_blank(),
          axis.line.x = NULL, 
          axis.line.y = NULL, 
          axis.text = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.x.top = element_blank(),
          axis.text.y = element_blank(), 
          axis.text.y.right = element_blank(),
          axis.ticks = element_blank(), 
          axis.ticks.length = unit(0, "pt"), 
          axis.title.x = element_blank(), 
          axis.title.x.top = element_blank(),
          axis.title.y = element_blank(), 
          axis.title.y.right = element_blank(),
          legend.background = element_blank(), 
          legend.spacing = unit(0.4, "cm"), 
          legend.spacing.x = NULL, 
          legend.spacing.y = NULL,
          legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
          legend.key = element_blank(), 
          legend.key.size = unit(1.2, "lines"), 
          legend.key.height = NULL, 
          legend.key.width = NULL,
          legend.text = element_text(size = rel(0.8)), 
          legend.text.align = NULL,
          legend.title = element_text(hjust = 0), 
          legend.title.align = NULL,
          legend.position = "none", 
          legend.direction = NULL,
          legend.justification = "center", 
          legend.box = NULL,
          legend.box.margin = margin(0, 0, 0, 0, "cm"), 
          legend.box.background = element_blank(),
          legend.box.spacing = unit(0.4, "cm"), 
          panel.background = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.spacing = unit(0, "pt"), 
          panel.spacing.x = NULL, 
          panel.spacing.y = NULL,
          panel.ontop = FALSE, 
          strip.background = element_blank(),
          strip.text = element_blank(), 
          strip.text.x = element_blank(),
          strip.text.y = element_blank(), 
          strip.placement = "inside",
          strip.placement.x = NULL, 
          strip.placement.y = NULL,
          strip.switch.pad.grid = unit(0, "cm"), 
          strip.switch.pad.wrap = unit(0, "cm"), 
          plot.background = element_blank(), 
          plot.title = element_blank(),
          plot.subtitle = element_blank(), 
          plot.caption = element_blank(),
          plot.margin = margin(0, 0, 0, 0), 
          complete = TRUE
        )
    }
    
    p <- ggplot(data = data.frame(x = 0:1, y = 0:1), aes_string(x = "x", y = "y")) +
      scale_x_continuous(limits = xlim, expand = c(0, 0)) +
      scale_y_continuous(limits = ylim, expand = c(0, 0)) +
      theme_nothing() +
      theme(
        plot.background = element_rect(colour = base_colour, fill = base_colour)
      ) +
      labs(x = NULL, y = NULL)
    if (!is.null(plot)) {
      p <- p + draw_plot(plot)
    }
    p
  }
  
  if (is.null(legend.position)) {
    legend.position <- c(
      which.max(c(grid.h.ratio[1], 0, grid.h.ratio[3])), 
      which.max(c(grid.v.ratio[1], 0, grid.v.ratio[3]))
    )
  }
  top.height <- grid.v.ratio[1]
  center.height <- grid.v.ratio[2]
  bottom.height <- grid.v.ratio[3]
  left.width <- grid.h.ratio[1]
  center.width <- grid.h.ratio[2]
  right.width <- grid.h.ratio[3]
  row.n <- 3
  col.n <- 3

  grid.newpage()
  grid_layout <- grid.layout(
    nrow = row.n,
    ncol = col.n,
    widths = unit(c(left.width, center.width, right.width), "null"),
    heights = unit(c(top.height, center.height, bottom.height), "null")
  )
  pushViewport(viewport(layout = grid_layout))
  print(ggdraw(), vp = viewport(layout.pos.col = 1, layout.pos.row = 1))
  print(ggdraw(), vp = viewport(layout.pos.col = 1, layout.pos.row = 2))
  print(ggdraw(), vp = viewport(layout.pos.col = 1, layout.pos.row = 3))
  print(ggdraw(), vp = viewport(layout.pos.col = 2, layout.pos.row = 1))
  print(ggdraw(), vp = viewport(layout.pos.col = 2, layout.pos.row = 2))
  print(ggdraw(), vp = viewport(layout.pos.col = 2, layout.pos.row = 3))
  print(ggdraw(), vp = viewport(layout.pos.col = 3, layout.pos.row = 1))
  print(ggdraw(), vp = viewport(layout.pos.col = 3, layout.pos.row = 2))
  print(ggdraw(), vp = viewport(layout.pos.col = 3, layout.pos.row = 3))

  ggplot2:::print.ggplot(data$left_item, vp = viewport(layout.pos.col = 1, layout.pos.row = 2))
  ggplot2:::print.ggplot(data$top_item, vp = viewport(layout.pos.col = 2, layout.pos.row = 1))
  ggplot2:::print.ggplot(data$right_item, vp = viewport(layout.pos.col = 3, layout.pos.row = 2))
  ggplot2:::print.ggplot(data$bottom_item, vp = viewport(layout.pos.col = 2, layout.pos.row = 3))
  ggplot2:::print.ggplot(data$center_item, vp = viewport(layout.pos.col = 2, layout.pos.row = 2))
  pushViewport(viewport(layout.pos.col = legend.position[1], layout.pos.row = legend.position[2]))
  grid.draw(data$hm.legend)
  return(invisible(upViewport(0)))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ggheatmap PARAM_DESCRIPTION
#' @param size PARAM_DESCRIPTION, Default: 24
#' @param barwidth PARAM_DESCRIPTION, Default: 10
#' @param barheight PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname edit.hm.legend
#' @export
edit.hm.legend <- function(ggheatmap, size = 24, barwidth = 10, barheight = 3) {
  set_colour <- function() {
    ggplot2::theme_get()$plot.background$colour %>%
      grDevices::col2rgb() %>%
      range() %>%
      sum() %>%
      (function(.x) {
        if ((.x * 100 * 0.5 / 255) >= 50) {
          "black"
        } else {
          "white"
        }
      })()
  }
  default_colour <- set_colour()
  hm.leg <- ggheatmap$center_item +
    theme(
      legend.position = c(0.50, 0.50),
      legend.text = element_text(colour = default_colour),
      legend.title = element_text(colour = default_colour)
    ) +
    guides(fill = guide_colourbar(
      title = "Spearman Correlation",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = barwidth,
      barheight = barheight,
      direction = "horizontal",
      title.theme = element_text(size = rel(size), angle = 0, colour = default_colour),
      label.theme = element_text(size = rel(size * 2 / 3), angle = 0, colour = default_colour)
    ))
  hm.leg <- ggplot_gtable(ggplot_build(hm.leg))
  hm.legend <- hm.leg$grobs[[which(sapply(hm.leg$grobs, function(x) x$name) == "guide-box")]]
  ggheatmap$hm.legend <- hm.legend
  return(ggheatmap)
}
