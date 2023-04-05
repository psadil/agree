.sem <- function(x, scale = 1) {
  sqrt(scale * stats::var(x) / length(x))
}

.ci <- function(x, scale = 1) {
  n <- length(x)
  standard_error <- .sem(x, scale = scale)
  avg <- mean(x)
  tibble::tibble(
    lower = avg - stats::qt(0.975, df = n - 1) * standard_error,
    upper = avg + stats::qt(0.975, df = n - 1) * standard_error
  )
}

.both <- function(center, shift) {
  tibble::tibble(
    lower = center - shift,
    upper = center + shift
  )
}

summary_tbl <- function(d, .dependentvar, .by = NULL) {
  by <- enquo(.by)

  d |>
    dplyr::summarise(
      dplyr::across(
        {{ .dependentvar }},
        .ci,
        .unpack = ".mean_{inner}"
      ),
      dplyr::across(
        {{ .dependentvar }},
        list(
          "n" = length,
          "mean" = mean,
          "var" = stats::var
        ),
        .names = ".{.fn}"
      ),
      dplyr::across(
        .data$.mean,
        \(x) .both(x, shift = 2 * sqrt(.data$.var)),
        .unpack = ".{inner}"
      ),
      dplyr::across(
        c(.data$.lower, .data$.upper),
        \(x) .both(x, shift = sqrt(3 * .data$.var / .data$.n)),
        .unpack = "{outer}_{inner}"
      ),
      .by = !!by
    )
}

StatBA <- ggplot2::ggproto(
  "StatBA",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, na.rm = FALSE) {
    summaries <- summary_tbl(data, y)

    data$y <- summaries$.mean
    data$y_lower <- summaries$.mean_lower
    data$y_upper <- summaries$.mean_upper

    data$ymin <- summaries$.lower_lower
    data$lower <- summaries$.lower
    data$lower_upper <- summaries$.lower_upper

    data$ymax <- summaries$.upper_upper
    data$upper_lower <- summaries$.upper_lower
    data$upper <- summaries$.upper

    # Remove rows with missing data
    if (na.rm) {
      data <- data[complete.cases(data), ]
    }

    data
  },
  required_aes = c("x", "y")
)


stat_ba <- function(
    mapping = NULL,
    data = NULL,
    geom = "BA",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...) {
  ggplot2::layer(
    stat = StatBA,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



GeomBA <- ggplot2::ggproto(
  "GeomBA",
  ggdist::GeomLineribbon,
  default_aes = ggplot2::aes(
    colour = "black",
    fill = "blue",
    linetype = 1,
    alpha = 0.2
  ),
  draw_panel = function(
      self,
      data,
      ...) {
    center <- data |>
      dplyr::mutate(ymin = y_lower, ymax = y_upper) |>
      dplyr::arrange(x)

    lower <- data |>
      dplyr::mutate(y = lower, ymax = lower_upper, fill = "red") |>
      dplyr::arrange(x)

    upper <- data |>
      dplyr::mutate(y = upper, ymin = upper_lower, fill = "red") |>
      dplyr::arrange(x)

    grobs <- grid::gList(
      ggdist::GeomLineribbon$draw_panel(center, ...),
      ggdist::GeomLineribbon$draw_panel(lower, ...),
      ggdist::GeomLineribbon$draw_panel(upper, ...)
    )
    grobs
    # grid::gTree(children = grobs)
  }
)

#' Bland-Altman Agreement Plot
#'
#' @param mapping Set of aesthetic mappings created by aes(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer
#' @param stat Generally unused
#' @param position Generally unused
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @export
#'
#' @examples
#' d <- data.frame(x = rnorm(100), y = rnorm(100)) |>
#'   dplyr::mutate(
#'     .difference = x - y,
#'     .average = (x + y) / 2
#'   )
#' d |>
#'   ggplot2::ggplot(ggplot2::aes(x = .average, y = .difference)) +
#'   geom_ba()
#'
#' d$group <- rep(c(1, 2), each = 50)
#'
#' d |>
#'   ggplot2::ggplot(ggplot2::aes(x = .average, y = .difference)) +
#'   ggplot2::facet_wrap(~group) +
#'   geom_ba()
#'
geom_ba <- function(
    mapping = NULL,
    data = NULL,
    stat = "BA",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...) {
  ggplot2::layer(
    geom = GeomBA,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
