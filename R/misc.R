#' Return a function to determine pretty integer break
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Modified from [https://joshuacook.netlify.app/post/integer-values-ggplot-axis/](https://joshuacook.netlify.app/post/integer-values-ggplot-axis/)
#'
#' @param n A integer. Number of breaks.
#' @param ... Supplementary arguments passed to base::pretty()
#' @export
#' @examples
#' # from 0 to 2
#' patchwork::wrap_plots(
#'   scales::demo_continuous(c(0, 2)),
#'   scales::demo_continuous(c(0, 2), breaks = integer_breaks()),
#'   nrow = 2
#' )
#'
#' # from 0 to 10.5
#' patchwork::wrap_plots(
#'   scales::demo_continuous(c(0, 10.5)),
#'   scales::demo_continuous(c(0, 10.5), breaks = integer_breaks()),
#'   scales::demo_continuous(c(-10.5, 0), breaks = integer_breaks()),
#'   nrow = 3
#' )
#'
#' # Choose only the maximum or minimum integer.
#' patchwork::wrap_plots(
#'   scales::demo_continuous(c(0, 10.5), breaks = \(x) tail(integer_breaks()(x), 1)),
#'   scales::demo_continuous(c(-10.5, 0), breaks = \(x) head(integer_breaks()(x), 1)),
#'   nrow = 2
#' )
#'
#' # Tricky behavior. It is likely due to the `pretty()` choses 1, 2 or 5 times a power of 10.
#' str(lapply(1:20, \(x) integer_breaks(0)(c(0, x))))
#'
integer_breaks <- function(n = 5, ...) {
  function(x) {
    breaks <- floor(pretty(x, n, ...))
    breaks <- unique(breaks)
    breaks <- breaks[breaks <= max(x, na.rm = TRUE)]
    breaks <- breaks[breaks >= min(x, na.rm = TRUE)]
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
}
