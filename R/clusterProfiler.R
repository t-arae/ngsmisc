
#' Run clustreProfiler::enrichGO() and write R object to a file
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

#' Check the ego contains significantly enriched GO term
#' @param ego An enrichResult class object.
#'
clP_is_significant <- function(ego) {
  any(ego@result[["qvalue"]] < ego@qvalueCutoff, na.rm = TRUE)
}

#' Return the number of significantly enriched GO term in the ego
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

#' Wrap strings to defined width.
#' @param string A vector of strings.
#' @param width A number of string width to wrap. (default: 40)
#'
wrap_strings <- function(string, width = 40) {
  as.character(string) %>% stringr::str_wrap(width = width)
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
#' @inheritParams wrap_strings
#' @export
#'
clP_plot_bar <- function(li_ego, n = 10, width = 40) {
  li_gp <- list()
  for(ont in c("BP", "CC", "MF")) {
    if(is.null(li_ego[[ont]])) {
      li_gp[[ont]] <- patchwork::plot_spacer()
    } else if (!clP_is_significant(li_ego[[ont]])) {
      li_gp[[ont]] <- patchwork::plot_spacer()
    } else {
      li_ego[[ont]]@result$Description <-
        wrap_strings(li_ego[[ont]]@result$Description, width = width)
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
#' @param scale_fill A function controlling fill color of bars. it must be compatible with ggplot2::scale_fill_*(). default: ggplot2::scale_fill_viridis_c
#' @export
#'
clP_wrap_barplots <- function(li_gp, scale_fill = ggplot2::scale_fill_viridis_c) {
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
    scale_fill(limits = fill_range)
  pgp
}

#' Write the enrichGO result to a barplot
#' @inheritParams clP_path2label
#' @inheritParams clP_plot_bar
#' @inheritParams clP_wrap_barplots
#' @param file_suffix output file suffix. default: "png"
#' @param plot_width plot width. default: 15 (in)
#' @param plot_height plot height. default: 6 (in)
#' @param ... Supplementary arguments passed to ggplot2::ggsave()
#' @export
#'
clP_write_li_ego_as_bar <- function(
  path_li_ego_rds,
  file_suffix = "png",
  n = 10,
  width = 40,
  scale_fill = ggplot2::scale_fill_viridis_c,
  plot_width = 15,
  plot_height = 6,
  ...
) {
  li_ego <- readRDS(path_li_ego_rds)
  label <- clP_path2label(path_li_ego_rds)
  out_dir <- fs::path_dir(fs::path_dir(path_li_ego_rds))

  fs::dir_create(fs::path(out_dir, "barplot"))
  outf <- fs::path(out_dir, "barplot", stringr::str_glue("barplot_{label}.{file_suffix}"))
  pgp <-
    li_ego %>%
    clP_plot_bar(n = n, width = width) %>%
    clP_wrap_barplots(scale_fill = scale_fill)
  if(is.null(pgp)) {
    message(stringr::str_glue("li_ego_{label} has no significant enrichment."))
    return()
  }
  ggplot2::ggsave(outf, pgp, width = plot_width, height = plot_height, ...)
}
