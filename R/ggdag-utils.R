require_tidy_dagitty <- function(x) {
  assertthat::assert_that(
    "tidy_dagitty" %in% class(x),
    msg = "This operation only works with objects created with ggdag::tidy_dagitty"
    )
  invisible(x)
}
subscriptify_node_labels <- function(x, start = 'u') {
  require_tidy_dagitty(x)
  x$data <- dplyr::mutate(
    x$data, label = stringr::str_replace(
      name, stringr::str_c(start, "([a-zA-Z])"),
      stringr::str_c(start, "\\[\\1\\]")))
  x
}
#' @export
rename_node_labels <- function(.tidy_dag, ...) {
  require_tidy_dagitty(.tidy_dag)
  r <- rlang::enquos(...)
  values <- map_chr(r, ~rlang::as_label(.x))
  names(values) <- names(r)
  .tidy_dag$data <- dplyr::mutate(
    .tidy_dag$data,
    label = plyr::revalue(label, replace = values))
  .tidy_dag
}
rotate_dag <- function(x, rads = 0) {
  require_tidy_dagitty(x)
  if (rads == 0) {
    return (x)
  }
  x$data <- dplyr::mutate(
    x$data,
    .r = sqrt(x^2 + y^2),
    .rend = sqrt(xend^2 + yend^2),
    .th = atan2(y, x),
    .thend = atan2(yend, xend),
    x = .r * cos(.th + rads),
    xend = .rend * cos(.thend + rads),
    y = .r * sin(.th + rads),
    yend = .rend *  sin(.thend + rads)
  ) %>%
  select(-.r, -.rend, -.th,  -.thend)
  x
}
unitize_dag_df <- function(x, rads = 0) {
  unitize <- function(x) {
    xrange <- range(x$data$x)
    yrange <- range(x$data$y)
    diffx <- diff(xrange)
    diffy <- diff(yrange)
    diffx <- ifelse(diffx == 0, 1, diffx)
    diffy <- ifelse(diffy == 0, 1, diffy)
    x$data <- dplyr::mutate(
      x$data,
      x = (x - xrange[1]) / diffx,
      xend = (xend - xrange[1]) / diffx,
      y = (y - yrange[1]) / diffy,
      yend = (yend -  yrange[1]) / diffy
    ) %>%
      dplyr::mutate_at(dplyr::vars(x, xend, y, yend), ~.x * 2 - 1)
    x
  }
  x <- unitize(x)
  if (rads != 0) {
    x <- rotate_dag(x, rads)
    x <- unitize(x)
  }
  x
}
#' @export
augment <- function(x, node_fun, colify = FALSE, ...) {
  as_factor <- any(stringr::str_detect(names(formals(node_fun)), 'as_factor'))
  as.factor <- any(stringr::str_detect(names(formals(node_fun)), 'as\\.factor'))
  if (as_factor) {
    x <- node_fun(x, as_factor = FALSE, ...)
  } else {
    if (as.factor) {
      x <- node_fun(x, as.factor = FALSE, ...)
    } else {
      x <- node_fun(x)
    }
  }
  if (colify) {
    x <- colify(x, name = NULL, .name = colnames(x$data)[ncol(x$data)])
  }
  x
}
colify <- function(x, name, .name = NULL)  {
  if (!is.null(.name))
    name <- .name
  else
    name <- rlang::as_name(rlang::enquo(name))
  nc <- ncol(x$data)
  x$data <- x$data %>%
    dplyr::mutate(one = 1) %>%
    tidyr::pivot_wider(names_from = !!name, values_from = one)
  x$data <- x$data %>%
    dplyr::mutate_at(seq(nc, ncol(x$data)),  ~1 == dplyr::coalesce(.x, 0)) %>%
    dplyr::select(-dplyr::matches('^NA$', ignore.case = FALSE))
  x
}
#' @export
tidy_dagitty2 <- function(g, rads = 0, seed = 1) {
  x <- ggdag::tidy_dagitty(g, seed = seed, layout = "sugiyama") %>%
    augment(node_status, colify = TRUE)
  if (!any('latent' %in% colnames(x$data)))
    x$data <- dplyr::mutate(x$data, latent = FALSE)
  x$data <- dplyr::mutate(x$data, linetype_dashed = dplyr::coalesce(
    direction == "<->", FALSE))
  subscriptify_node_labels(x) %>%
  unitize_dag_df(rads)
}
#' @export
move_node <- function(.tidy_dag, .node, .x = NULL, .y = NULL) {
  require_tidy_dagitty(.tidy_dag)
  .node <- rlang::as_label(rlang::enquo(.node))
  .tidy_dag$data <- mutate(
    .tidy_dag$data,
    x = ifelse(!is.null(.x) & name == .node, .x, x),
    y = ifelse(!is.null(.y) & name == .node, .y, y),
    xend = ifelse(!is.null(.x) & to == .node, .x, xend),
    yend = ifelse(!is.null(.y) & to == .node, .y, yend)
    )
  .tidy_dag
}
#' @export
color_path <- function(.tidy_dag, p, color = 'red', base_color = gray(.5)) {
  require_tidy_dagitty(.tidy_dag)
  .tidy_dag$data <-
    c(stringr::str_extract_all(p, "[A-Za-z]+ <?->? [A-Za-z]+"),
    stringr::str_extract_all(stringr::str_remove(p, "[A-Za-z]+ <?->? "), "[A-Za-z]+ <?->? [A-Za-z]+")) %>%
    unlist() %>%
    map_chr(function(x) {if (stringr::str_detect(x, "<-")) {stringi::stri_reverse(x)} else {x}}) %>%
    tibble(path = ., color_path = color) %>%
    separate(path, c('name', 'direction', 'to'), sep = ' ') %>%
    select(-direction) %>%
    right_join(.tidy_dag$data) %>%
    mutate(color_path = coalesce(color_path, base_color))
  .tidy_dag
}
#' @export
smoke_ggplot2_base <- function(d, ...) {
  ggplot2::ggplot(d, ggplot2::aes(x, y, xend = xend, yend = yend, ...)) +
    ggdag::theme_dag() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5)) +
    ggplot2::coord_fixed()
}
#' @export
geom_dag_point2 <- function(...) {
  list(ggdag::geom_dag_point(
    mapping = ggplot2::aes(shape = latent),
    size = 2,
    show.legend = FALSE,
    stroke = 1),
    ggplot2::scale_shape_manual(values = c(19, 21)))
}
geom_dag_edges2_int <- ggdag::geom_dag_edges
body(geom_dag_edges2_int) <- rlang::parse_expr(
  stringr::str_replace(rlang::expr_text(body(ggdag::geom_dag_edges)),
                       "\\.\\.\\.\\)\\)",
                       "edge_linetype = 2, ...))"))
#' @export
geom_dag_edges2 <- purrr::partial(
  geom_dag_edges2_int,
  edge_width = .5,
  arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "closed"),
  arrow_bidirected = grid::arrow(length = grid::unit(8, "pt"), type = "closed", ends = "both")
)

#' @export
geom_dag_text2 <- purrr::partial(
  ggdag::geom_dag_text,
  mapping = ggplot2::aes(label = label),
  parse = TRUE,
  colour = 'black',
  size = 5
)
#' @export
smoke_ggplot2_basic_dag <- function(d, nudge_x = 0, nudge_y = .15) {
  smoke_ggplot2_base(d) +
    geom_dag_point2() +
    geom_dag_edges2() +
    geom_dag_text2(nudge_x = nudge_x, nudge_y = nudge_y)
}