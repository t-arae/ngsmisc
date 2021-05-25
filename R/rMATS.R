

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

#' Read rMATS output file (.MATS.JC.txt/.MATS.JCEC.txt)
#' @param fpath path to file
#' @export
#'
rM_read_MATS_test_out <- function(fpath) {
  cn_list <- c(MATS.JC_COLNAMES, MATS.JCEC_COLNAMES)
  ct_list <- c(MATS.JC_COLTYPES, MATS.JCEC_COLTYPES)
  temp_file <- fs::path_file(path = fpath)

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
