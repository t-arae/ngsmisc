##### rMATS.(JC|JCEC).txt column specs ----------------------------------------

return_MATS_COLUMN <- function(...) {
  part1 <- c("ID", "GeneID", "geneSymbol", "chr", "strand")
  part2 <-
    c("ID", "IJC_SAMPLE_1", "SJC_SAMPLE_1",  "IJC_SAMPLE_2","SJC_SAMPLE_2",
      "IncFormLen", "SkipFormLen", "PValue", "FDR",
      "IncLevel1", "IncLevel2", "IncLevelDifference")
  c(part1, ..., part2)
}

MATS.JC_COLNAMES <-
  list(
    A3SS.MATS.JC.txt =
      return_MATS_COLUMN("longExonStart_0base", "longExonEnd", "shortES",
                         "shortEE", "flankingES", "flankingEE"),
    A5SS.MATS.JC.txt =
      return_MATS_COLUMN("longExonStart_0base", "longExonEnd", "shortES",
                         "shortEE", "flankingES", "flankingEE"),
    MXE.MATS.JC.txt =
      return_MATS_COLUMN("1stExonStart_0base", "1stExonEnd", "2ndExonStart_0base",
                         "2ndExonEnd", "upstreamES", "upstreamEE",
                         "downstreamES", "downstreamEE"),
    RI.MATS.JC.txt =
      return_MATS_COLUMN("riExonStart_0base", "riExonEnd", "upstreamES",
                         "upstreamEE", "downstreamES", "downstreamEE"),
    SE.MATS.JC.txt =
      return_MATS_COLUMN("exonStart_0base", "exonEnd", "upstreamES",
                         "upstreamEE", "downstreamES", "downstreamEE")
  )

MATS.JC_COLTYPES <-
  list(
    A3SS.MATS.JC.txt = "icccciiiiiiicccciiddccd",
    A5SS.MATS.JC.txt = "icccciiiiiiicccciiddccd",
    MXE.MATS.JC.txt = "icccciiiiiiiiicccciiddccd",
    RI.MATS.JC.txt = "icccciiiiiiicccciiddccd",
    SE.MATS.JC.txt = "icccciiiiiiicccciiddccd"
  )

MATS.JCEC_COLNAMES <-
  MATS.JC_COLNAMES %>%
  purrr::set_names(~ stringr::str_replace(.x, "JC", "JCEC"))

MATS.JCEC_COLTYPES <-
  MATS.JC_COLTYPES %>%
  purrr::set_names(~ stringr::str_replace(.x, "JC", "JCEC"))


##### functions for manipulate rMATS.(JC|JCEC).txt -----------------------------

#' Read rMATS output file (.MATS.JC.txt/.MATS.JCEC.txt)
#' @param fpath path to file
#' @param ... ignored
#' @param convert_fpath a function convert to file name
#' @param extra_col named vector of additional column names and column spec. characters
#' @export
#'
rM_read_MATS_test_out <- function(fpath, ..., convert_fpath, extra_col) {
  cn_list <- c(MATS.JC_COLNAMES, MATS.JCEC_COLNAMES)
  ct_list <- c(MATS.JC_COLTYPES, MATS.JCEC_COLTYPES)

  if(!missing(extra_col)) {
    cn_list <- purrr::map(cn_list, ~ c(.x, names(extra_col)))
    ct_list <- purrr::map(ct_list, ~ paste0(c(.x, extra_col), collapse = ""))
  }

  if(missing(convert_fpath)) {
    temp_file <- fs::path_file(path = fpath)
  } else {
    temp_file <- convert_fpath(fpath)
  }

  if(!any(names(cn_list) %in% temp_file)) stop("invalid filename")

  # Check column name
  cn <- cn_list[[temp_file]]
  header <-
    readr::read_lines(file = fpath, n_max = 1L) %>%
    strsplit(split = "\t")
  if(!identical(cn, header[[1]])) stop("found column name mismatch")

  df <- suppressWarnings(
    readr::read_tsv(file = fpath, col_types = ct_list[[temp_file]])
  )
  # colnames(df) <- cn
  df
}

##### functions for plotting ---------------------------------------------------

#' Filter MATS tibble list by AGI
#' @param tbl_MATS A MATS event tibble
#' @param AGI Character vector containing AGI interested
#' @export
rM_filter_by_AGI <- function(tbl_MATS, AGI) {
  GeneID <- NULL
  dplyr::filter(tbl_MATS, GeneID %in% AGI)
}

rM_filter_by_count <- function(tbl_MATS, f) {
  counts <-
    tbl_MATS[, 7:10] %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    lapply(function(x) paste(unlist(x), collapse = ",")) %>%
    stringr::str_split(",") %>%
    lapply(as.integer)

  dplyr::filter(tbl_MATS, purrr::map_lgl(counts, ~ f(.x))
}

#' Extract event type from file path
#' @param path file path
#' @export
rM_extract_event_type <- function(path) {
  stringr::str_extract(fs::path_file(path), "SE|A[35]SS|MXE|RI")
}

#' Select some columns to plotting
#' @inheritParams rM_filter_by_AGI
#' @param event_type AS event type
#' @export
rM_select_switch_start_end_col <- function(tbl_MATS, event_type) {
  ID <- GeneID <- chr <- strand <- IJC_SAMPLE_1 <- SJC_SAMPLE_1 <-
    IJC_SAMPLE_2 <- SJC_SAMPLE_2 <- IncFormLen <- SkipFormLen <-
    PValue <- FDR <- IncLevel1 <- IncLevel2 <- IncLevelDifference <-
    start <- end <- NULL

  start_end <-
    dplyr::case_when(
      event_type == "SE" ~ c("exonStart_0base", "exonEnd"),
      event_type == "A5SS" ~ c("longExonStart_0base", "longExonEnd"),
      event_type == "A3SS" ~ c("longExonStart_0base", "longExonEnd"),
      event_type == "MXE" ~ c("1stExonStart_0base", "1stExonEnd"),
      event_type == "RI" ~ c("upstreamEE", "downstreamES")
    )
  dplyr::select(
    tbl_MATS, ID, GeneID, chr, strand,
    start = start_end[1], end = start_end[2],
    IJC_SAMPLE_1, SJC_SAMPLE_1, IJC_SAMPLE_2, SJC_SAMPLE_2,
    IncFormLen, SkipFormLen, PValue, FDR,
    IncLevel1, IncLevel2, IncLevelDifference
  ) %>%
    dplyr::mutate(
      ID  = ID,
      start = start,
      end = end,
      IncFormLen = IncFormLen,
      SkipFormLen = SkipFormLen,
      PValue = PValue,
      FDR = FDR,
      IncLevelDifference = IncLevelDifference,
      event_type
    )
}

#' Create tibble containing splice site info from tbl_MATS
#' @inheritParams rM_select_switch_start_end_col
#' @export
rM_make_tbl_splice_site <- function(tbl_MATS, event_type) {
  shortEE <- longExonEnd <- flankingES <- IncLevel1 <- IncLevel2 <-
    se1 <- coord <- name <- variant <- upstreamEE <- exonEnd <-
    exonStart_0base <- downstreamES <- ss11 <- se11 <- se21 <- NULL

  # A5SS and A3SS
  if(any(event_type %in% c("A5SS", "A3SS"))) {
    tbl_temp <-
      dplyr::select(tbl_MATS,
                    ss1 = shortEE, ss2 = longExonEnd, se1 = flankingES,
                    IncLevel1, IncLevel2) %>%
      dplyr::mutate(se1 = se1 + 1L, se2 = se1) %>%
      tidyr::pivot_longer(cols = -c(IncLevel1, IncLevel2), values_to = "coord") %>%
      dplyr::mutate(
        coord = as.integer(coord),
        variant = stringr::str_sub(name, -1),
        g = variant
      )
  } else if(event_type == "SE") {
    tbl_temp <-
      dplyr::select(tbl_MATS,
                    ss11 = upstreamEE, ss21 = exonEnd,
                    se11 = exonStart_0base, se21 = downstreamES,
                    IncLevel1, IncLevel2) %>%
      dplyr::mutate(ss12 = ss11,
                    se11 = se11 + 1L,
                    se21 = se21 + 1L,
                    se12 = se21) %>%
      tidyr::pivot_longer(cols = -c(IncLevel1, IncLevel2), values_to = "coord") %>%
      dplyr::mutate(
        coord = as.integer(coord),
        variant = stringr::str_sub(name, -1),
        g = stringr::str_sub(name, -2, -1)
      )
  } else if(event_type == "RI") {
    tbl_temp <-
      dplyr::select(tbl_MATS, ss1 = upstreamEE, se1 = downstreamES,
                    IncLevel1, IncLevel2) %>%
      # purrr::modify_at(1:2, as.integer) %>%
      dplyr::mutate(se1 = se1 + 1L) %>%
      tidyr::pivot_longer(cols = -c(IncLevel1, IncLevel2), values_to = "coord") %>%
      dplyr::mutate(
        coord = as.integer(coord),
        variant = "skip intron",
        g = variant
      )
  }
  return(tbl_temp)
}


# rM_read_MATS_test_out(inf) %>% dplyr::glimpse()
#
#
# "~/Dropbox/ngs_analysis/fujita_etal_dcmu_dbmib/analysis/rmats_out/dmso_dbmib_0/" %>%
#   # fs::dir_ls(regexp = "fromGTF.[AMRS]") %>%
#   fs::dir_ls(regexp = "fromGTF.[n]") %>%
#   .[1] %>%
#   lapply(readr::read_tsv) %>%
#   .[[1]] %>%
#   # dplyr::filter(ID > 62) %>%
#   dplyr::glimpse()
