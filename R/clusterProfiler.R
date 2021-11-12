
#' Run clustreProfiler::enrichGO() and write R object of the result to a file
#' @param fg A vector of foreground gene ids.
#' @param bg A vector of background gene ids.
#' @param out_dir Path to the output directory.
#' @param label A label of the set of foreground gene
#' @param ... Named options will be passed to clusterProfiler::enrichGO(). without "gene", "universe", "ont", "pool"
#' @export
#'
clP_write_li_ego <- function(fg, bg, out_dir, label, ...) {
  fs::dir_create(out_dir, "ego_rds")
  cat(paste0("Now processing '", label, "'\n"))
  li_ego <-
    list(
      BP = clusterProfiler::enrichGO(gene = fg, universe = bg, ont = "BP", ...),
      CC = clusterProfiler::enrichGO(gene = fg, universe = bg, ont = "CC", ...),
      MF = clusterProfiler::enrichGO(gene = fg, universe = bg, ont = "MF", ...)
    )
  saveRDS(li_ego, fs::path(out_dir, "ego_rds", paste0("li_ego_", label, ".rds")))
}

#' Extract label of gene set from the file path of result list
#' @param path_li_ego_rds Path to RDS files of the li_ego object.
#'
clP_path2label <- function(path_li_ego_rds) {
  fs::path_file(path_li_ego_rds) %>%
    stringr::str_remove("^li_ego_") %>%
    stringr::str_remove(".rds$")
}

#' Check the ego contains significantly enriched GOterm
#' @param ego An enrichResult class object.
#'
clP_is_significant <- function(ego) {
  any(ego@result[["qvalue"]] < ego@qvalueCutoff, na.rm = TRUE)
}

#' Return the number of significantly enriched GOterm in the ego
#' @inheritParams clP_is_significant
#'
clP_num_significant <- function(ego) {
  sum(ego@result[["qvalue"]] < ego@qvalueCutoff, na.rm = TRUE)
}

#' Write the enrichGO result to a csv file
#' @inheritParams clP_path2label
#' @export
#'
clP_write_li_ego_as_csv <- function(path_li_ego_rds) {
  li_ego <- readRDS(path_li_ego_rds)
  label <- clP_path2label(path_li_ego_rds)
  out_dir <- fs::path_dir(fs::path_dir(path_li_ego_rds))
  for(ont in c("BP", "CC", "MF")) {
    fs::dir_create(fs::path(out_dir, ont))
    outf <- fs::path(out_dir, ont, stringr::str_glue("result_{label}_{ont}.csv"))
    if(is.null(li_ego[[ont]])) {
      message(stringr::str_glue("li_ego_{label}${ont} is NULL. Skipped."))
      next
    }
    li_ego[[ont]]@result %>% tibble::as_tibble() %>% readr::write_csv(outf)
  }
}

#' Return a function to determin pretty integer break
#' https://www.r-bloggers.com/2019/11/setting-axes-to-integer-values-in-ggplot2/
#' @param n A integer. Number of breaks.
#' @param ... Supplementary arguments passed to base::pretty()
#'
integer_breaks <- function(n = 5, ...) {
  function(x) {
    breaks <- floor(pretty(x, n, ...))
    breaks <- unique(breaks)
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
}

#' Wrap strings to defined width.
#' @param string A vector of strings.
#' @param w A number of string width to wrap.
#'
wrap_strings <- function(string, w = 40) {
  as.character(string) %>% stringr::str_wrap(width = w)
}

#' Theme for the barplot
clP_gp_theme_bar <- function() {
  list(
    ggplot2::theme_linedraw(base_size = 12),
    ggplot2::scale_color_viridis_c(),
    ggplot2::scale_fill_viridis_c(),
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, .1)),
      breaks = integer_breaks()
    )
  )
}

#' Write ego result as barplot
#' @param li_ego A list of enrichResult-class objects
#' @param n A number of GOterms to show. (default=10)
#' @export
#'
clP_plot_bar <- function(li_ego, n = 10) {
  li_gp <- list()
  for(ont in c("BP", "CC", "MF")) {
    if(is.null(li_ego[[ont]])) {
      li_gp[[ont]] <- patchwork::plot_spacer()
    } else if (!clP_is_significant(li_ego[[ont]])) {
      li_gp[[ont]] <- patchwork::plot_spacer()
    } else {
      li_ego[[ont]]@result$Description <-
        wrap_strings(li_ego[[ont]]@result$Description)
      q_cutoff <- li_ego[[ont]]@qvalueCutoff
      num_sig_term <- clP_num_significant(li_ego[[ont]])
      sub <- ifelse(
        test = num_sig_term <= n,
        yes = stringr::str_glue("{num_sig_term} sig. terms"),
        no = stringr::str_glue("Top {n} of {num_sig_term} sig. terms")
      )

      li_gp[[ont]] <-
        enrichplot:::barplot.enrichResult(
          li_ego[[ont]],
          x = "Count",
          color = "qvalue",
          showCategory = n
        ) +
        clP_gp_theme_bar() +
        ggplot2::labs(title = ont, subtitle = sub, x = "Count")
    }
  }
  li_gp$q_cutoff <- ifelse(exists("q_cutoff"), q_cutoff, NA)
  return(li_gp)
}

#' Wrapping barplots of enrichResult-objects
#' @param li_gp list of ggplot objects
#' @param fill_fun A function to colorize bar
#' @export
#'
clP_wrap_barplots <- function(li_gp, fill_fun = ggplot2::scale_fill_viridis_c) {
  q_cutoff <- li_gp$q_cutoff
  if(is.na(q_cutoff)) return(NULL)

  ### 複数のplot間でレジェンドの色の範囲を揃えるために，最大値と最小値を取り出す。
  fill_range <-
    li_gp[names(li_gp) != "q_cutoff"] %>%
    purrr::keep(~ !inherits(.x, "spacer")) %>%
    purrr::map(~ range(.x$data$qvalue)) %>%
    unlist() %>%
    range()

  ### 各棒グラフを一つのプロットにまとめる
  pgp <-
    li_gp[names(li_gp) != "q_cutoff"] %>%
    patchwork::wrap_plots(guides = "collect") +
    patchwork::plot_annotation(caption = stringr::str_glue("qvalue < {q_cutoff}")) &
    fill_fun(limits = fill_range)
  pgp
}

#' Write the enrichGO result to a barplot
#' @inheritParams clP_path2label
#' @export
#'
clP_write_li_ego_as_bar <- function(path_li_ego_rds) {
  li_ego <- readRDS(path_li_ego_rds)
  label <- clP_path2label(path_li_ego_rds)
  out_dir <- fs::path_dir(fs::path_dir(path_li_ego_rds))

  fs::dir_create(fs::path(out_dir, "barplot"))
  outf <- fs::path(out_dir, "barplot", stringr::str_glue("barplot_{label}.png"))
  pgp <-
    li_ego %>%
    clP_plot_bar() %>%
    clP_wrap_barplots()
  if(is.null(pgp)) {
    message(stringr::str_glue("li_ego_{label} has no significant enrichemnt."))
    return()
  }
  ggplot2::ggsave(outf, pgp, width = 15, height = 6)
}
