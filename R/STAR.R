#' Parse STAR final.log files as a list
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Parse STAR final.log file as a list.
#'
#' @param fpath a path to the STAR final.log file
#'
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc", "star") %>%
#'   fs::dir_ls(regexp = ".final.log$")
#' ST_parse_final_log(infs[1]) %>% str(vec.len = 1)
#'
#' @export
ST_parse_final_log <- function(fpath) {
  temp <- NULL
  # Check the first of lines
  fl <- readLines(fpath, n = 1L)
  if(!stringr::str_detect(fl, "Started job on |"))
    stop(paste0("fpath must be the log file from STAR (*.final.log)"))

  lines <- readLines(fpath) %>% stringr::str_trim()
  parse <- function(x) {
    res <-
      lapply(x, function(x) {
        temp <- stringr::str_split(x, " [|]\\t", simplify = TRUE)[2]
      }) %>%
      purrr::modify(~ ifelse(grepl("^[0-9,.]+$", .x), as.numeric(.x), .x))
    names(res) <- lapply(x, function(x) {
      stringr::str_split(x, " [|]\\t", simplify = TRUE)[1]
    })
    res
  }

  results <- list()
  results[["Summary"]] <- parse(lines[c(1:4, 6:7)])
  results[["Unique Reads"]] <- parse(lines[9:22])
  results[["Multi-mapping Reads"]] <- parse(lines[24:27])
  results[["Unmapped Reads"]] <- parse(lines[29:34])
  results[["Chimeric Reads"]] <- parse(lines[36:37])

  results
}

#' Read STAR final.log files as tibbles and merge them
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Read STAR final.log files as tibbles and merge them.
#'
#' @param fpath a path to the STAR final.log file
#' @param li_tbl a list of data.frame
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc", "star") %>%
#'   fs::dir_ls(regexp = ".final.log$")
#' ST_read_final_log(infs[1]) %>% rename_fpath(nth = 3) %>% print(n = Inf)
#'
#' infs %>%
#'   lapply(ST_read_final_log) %>%
#'   lapply(rename_fpath, nth = 3, suffix = ".final.log") %>%
#'   ST_merge_final_log()
#'
#' @name ST_final_log
NULL

#' @rdname ST_final_log
#' @export
ST_read_final_log <- function(fpath) {
  temp <- NULL
  results <- ST_parse_final_log(fpath)

  return(
    tibble::tibble(
      contents_group =
        rep(names(results), c(6, 14, 4, 6, 2)) %>%
        forcats::fct_inorder(),
      contents =
        names(unlist(stats::setNames(results, NULL))) %>%
        forcats::fct_inorder(),
      temp =
        unlist(results)
    ) %>%
      dplyr::rename(!!fpath := temp)
  )
}

#' @rdname ST_final_log
#' @export
ST_merge_final_log <- function(li_tbl) {
  li_tbl %>%
    purrr::reduce(dplyr::left_join, by = c("contents_group", "contents"))
}

#' Read STAR SJ.out.tab file as a tibble
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Read (and decode data) STAR SJ.out.tab file as a tibble.
#'
#' @details
#' See the section 5.5 "Splice junctions" in the STAR manual.
#'
#' @param fpath a path to the STAR final.log file
#' @param decode logical. If `TRUE` decode values in 3rd-5th columns. (default: `FALSE`)
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc", "star") %>%
#'   fs::dir_ls(regexp = ".sj.tsv$")
#' ST_read_sj_tab(infs[1])
#' ST_read_sj_tab(infs[1], decode = TRUE)
#'
#' # it can be converted to the `GenomicRanges::GRanges-class` object
#' # ST_read_sj_tab(infs[1], decode = TRUE) %>% plyranges::as_granges()
#'
#' tbl_merge <-
#'   infs %>%
#'   lapply(ST_read_sj_tab, decode = TRUE) %>%
#'   lapply(head) %>%
#'   purrr::imap(~ dplyr::mutate(.x, sample = fs::path_file(.y))) %>%
#'   dplyr::bind_rows()
#' tbl_merge
#'
#' @name ST_sj_tab
NULL

#' @rdname ST_sj_tab
#' @export
ST_read_sj_tab <- function(fpath, decode = FALSE) {
  COLNAMES <- c("seqnames", "start", "end", "strand", "intron_motif",
                "annotated", "num_uniq_map_jc", "num_multi_map_jc",
                "max_overhang")
  tbl <- readr::read_tsv(fpath, col_names = COLNAMES, col_types = "ciiiiiiii")

  if(decode) {
    tbl <-
      tbl %>%
      dplyr::mutate(strand = dplyr::case_when(
        strand == 0L ~ "*",
        strand == 1L ~ "+",
        strand == 2L ~ "-"
      )) %>%
      dplyr::mutate(intron_motif = dplyr::case_when(
        intron_motif == 0L ~ "non-canonical",
        intron_motif == 1L ~ "GT/AG",
        intron_motif == 2L ~ "CT/AC",
        intron_motif == 3L ~ "GC/AG",
        intron_motif == 4L ~ "CT/GC",
        intron_motif == 5L ~ "AT/AC",
        intron_motif == 6L ~ "GT/AT"
      )) %>%
      dplyr::mutate(annotated = dplyr::case_when(
        annotated == 0L ~ "unannotated",
        annotated == 1L ~ "annotated"
      ))
  }
  return(tbl)
}
