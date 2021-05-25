
#'
#' library(magrittr)
#' library(bstringr)
#' library(ggplot2)
#'
#' ### Define functions for gff data.frame --------------------------------------
#'
#' GFF_COLNAME <-
#'   c("seqid", "source", "type", "start", "end",
#'     "score", "strand", "phase", "attributes")
#'
#' # GFFのattributes列の情報を別々の列に分割する
#' #' Separate the "attributes" column to individual columns
#' #' @param gff a gff data.frame
#' #'
#' gff_separate_attr <- function(gff) {
#'   col_name <-
#'     gff$attributes %>%
#'     stringr::str_extract_all("(^|;)[^=]+") %>%
#'     unlist %>%
#'     stringr::str_remove(";") %>%
#'     unique
#'   message("these column are added:\n", paste(col_name, collapse = ", "))
#'
#'   li <- list()
#'   for(i in col_name) {
#'     li[[i]] <- gff$attributes %>%
#'       stringr::str_extract(paste0(i, "[^;]+")) %>%
#'       stringr::str_remove_all(paste0(i, "=|;"))
#'   }
#'   dplyr::bind_cols(gff, tibble::as_tibble(li))
#' }
#'
#' #' Create "seq" sequence column to gff data.frame
#' #' @param gff a gff data.frame
#' #' @param ref_dstr dstr object
#' #' @param coord logical
#' #'
#' gff_mutate_seq <- function(gff, ref_dstr) {
#'     dplyr::mutate(gff, seq = stringr::str_sub(ref_dstr[seqid], start, end))
#' }
#'
#' #' Search rows containing AGI code in "attributes" column from gff data.frame
#' #' @param gff a gff data.frame
#' #' @param agi AGI code
#' #'
#' gff_filter_agi <- function(gff, agi) {
#'   dplyr::filter(gff, stringr::str_detect(attributes, agi))
#' }
#'
#' gff_filter_attributes <-
#'   function(gff, regex, attributes_tags = c("ID", "Parent")) {
#'     exgff <-
#'       gff %>%
#'       dplyr::mutate(rn = dplyr::row_number()) %>%
#'       dplyr::filter(stringr::str_detect(attributes, regex)) %>%
#'       gff_separate_attr()
#'
#'     purrr::map(attributes_tags, ~ stringr::str_detect(exgff[[.x]], regex)) %>%
#'       purrr::map(~ ifelse(is.na(.x), FALSE, TRUE)) %>%
#'       purrr::reduce(~ .x | .y) %>%
#'       {dplyr::filter(exgff, .)} %>%
#'       .$rn %>%
#'       {gff[.,]}
#'   }
#'
#' # 与えられたgff内のfeatureがお互いに重なりあっていないかを調べる
#' # 行数分のbooleanで返す。TRUEだったら重なったfeatureがある
#' #
#' gff_check_feature_overlap <- function(gff) {
#'   dplyr::select(gff, seqid, start, end) %>%
#'     dplyr::mutate(rn = dplyr::row_number()) %>%
#'     dplyr::rowwise() %>%
#'     dplyr::group_map(~ overlapped_feature(gff[-.x$rn,], .x$seqid, .x$start, .x$end)) %>%
#'     purrr::map_lgl(~ nrow(.x) > 0)
#' }
#'
#' # 与えられたgffのtype列がfilter_typeと一致する行をフィルタリングして，
#' # attributes列のsplit_attr_tagで指定したtagでデータフレームを分割する
#' #
#' gff_filter_split <- function(gff, filter_type = "exon", split_attr_tag = "Parent") {
#'   filtered_gff <- gff %>% dplyr::filter(type == filter_type)
#'   # if(0 != nrow(filtered_gff)) return(NULL)
#'   temp_tag <- gff_separate_attr(filtered_gff)[[split_attr_tag]]
#'   if(is.null(temp_tag)) stop(paste0("No attributes tag: ", split_attr_tag))
#'   split(filtered_gff, temp_tag)
#' }
#'
#' # 与えられたgffとref_dstrからfeatureの配列を抽出して，
#' # 向きを考慮してそれらをつなげたdstrを返す
#' # gffはstrandが一種類でかつfeatureが重ならず独立でないとだめ。
#' #
#' gff_extract_concat_dstr <- function(gff, ref_dstr) {
#'   if(any(gff_check_feature_overlap(gff))) stop()
#'
#'   coord <- unique(gff$strand)
#'   if(length(coord) != 1L) stop("multiple 'strand' types detected")
#'   if(coord == "+") {
#'     gff <- dplyr::arrange(gff, end) %>% gff_mutate_seq(ref_dstr)
#'   } else {
#'     gff <-
#'       dplyr::arrange(gff, desc(end)) %>%
#'       gff_mutate_seq(ref_dstr) %>%
#'       dplyr::mutate(seq = bstringr::dstr_rev_comp(seq))
#'   }
#'
#'   gff %>%
#'     {paste0(.$seq, collapse = "")} %>%
#'     bstringr::as_dstr()
#' }
#'
#'
#' # misc_fun
#' # gff_filter_splitした後に，gff_extract_concat_dstrする
#' # split_attr_tagを名前にしたdstrを返す
#' #
#' gff_extract_transcript_dstr <-
#'   function(gff, ref_dstr, filter_type = "exon", split_attr_tag = "Parent") {
#'     gff_filter_split(gff, filter_type = filter_type,
#'                      split_attr_tag = split_attr_tag) %>%
#'       purrr::map(~ gff_extract_concat_dstr(.x, ref_dstr = ref_dstr)) %>%
#'       purrr::imap(~ purrr::set_names(.x, .y)) %>%
#'       purrr::reduce(c)
#'   }
#'
#'
#' # hoge <- gff_filter_attributes(gff_tair, "AT1G02080")
#' # hoge <- gff_filter_attributes(gff_tair, "AT1G01020")
#' #
#' # hoge %>%
#' #   gff_filter_split("exon", "Parent") %>%
#' #   purrr::map(~ gff_extract_concat_dstr(.x, fasta_dstr))
#' #
#' # hoge %>%
#' #   gff_filter_split("exon", "Parent") %>%
#' #   purrr::map(~ gff_extract_concat_dstr(.x, fasta_dstr)) %>%
#' #   purrr::map(~ bstringr::dstr_find_orfs(.x)[[1]]) %>%
#' #   purrr::map(~ .x[which.max(nchar(.x))]) %>%
#' #   purrr::reduce(c)
#' #   purrr::map(~ sort(.x[[1]], by = "length", decreasing = T))
#' #
#' # hoge %>% gff_extract_transcript_dstr(fasta_dstr)
#' #
#' # hoge %>%
#' #   gff_extract_transcript_dstr(fasta_dstr) %>%
#' #   bstringr::dstr_find_orfs() %>%
#' #   purrr::map(~ .x[which.max(nchar(.x))]) %>%
#' #   purrr::reduce(c) %>%
#' #   bstringr::dstr_translate()
#' #
#' # hoge %>%
#' #   gff_extract_transcript_dstr(fasta_dstr, filter_type = "CDS") %>%
#' #   bstringr::dstr_translate()
#'
#'
#' ### !!!!! bstringr::bstr_sortのdecreasingがバグってる
#'
#' # exgff_extract_dstr_feature <- function(ex_gff, required_type) {
#' gff_extract_dstr_feature <- function(gene_gff, required_type) {
#'   coord <- unique(gene_gff$strand)
#'   if(length(coord) != 1L) stop("multiple 'strand' types detected")
#'
#'   df_li <-
#'     dplyr::filter(gene_gff, type == required_type) %>%
#'     split(.$Parent)
#'   if(coord == "-") df_li <- purrr::map(df_li, ~ dplyr::arrange(.x, desc(start)))
#'
#'   df_li <-
#'     df_li %>%
#'     purrr::map(.f = ~ bstringr::dstr(
#'       x = .x$seq,
#'       n = paste0(.x$Parent, ": ", required_type, 1:nrow(.x))
#'     ))
#'   if(coord == "-") df_li %>% purrr::map(~ dstr_rev_comp(.x))
#'   else df_li
#' }
#'
#' gff_extract_dstr <- function(gene_gff, fasta_dstr) {
#'   exgff <- gff_mutate_seq(gene_gff, fasta_dstr) %>% gff_separate_attr()
#'   coord <- exgff$strand[1]
#'
#'   gene <- dplyr::filter(exgff, type == "gene") %>%
#'     {bstringr::dstr(x = .$seq, n = .$ID)}
#'   if(coord == "-") gene <- bstringr::dstr_rev_comp(gene)
#'   exon_li <- gff_extract_dstr_feature(exgff, "exon")
#'   futr_li <- gff_extract_dstr_feature(exgff, "five_prime_UTR")
#'   tutr_li <- gff_extract_dstr_feature(exgff, "three_prime_UTR")
#'   cdna <- purrr::map(exon_li, ~ paste0(.x, collapse = "")) %>%
#'     unlist %>%
#'     bstringr::dstr()
#'
#'   names(futr_li) <- names(tutr_li) <- names(exon_li) <- NULL
#'   c(gene,
#'     cdna,
#'     bstringr::as_dstr(unlist(exon_li)),
#'     bstringr::as_dstr(unlist(futr_li)),
#'     bstringr::as_dstr(unlist(tutr_li)))
#' }
#'
#' overlapped_feature <- function(gff, q_seqid, q_start, q_end) {
#'   dplyr::filter(gff, seqid == q_seqid, start <= q_end, end >= q_start)
#' }
#'
#' overlapped_gff <- function(source_gff, query_gff, include_query = TRUE) {
#'   ordered_source_gff <- dplyr::mutate(source_gff, rn = dplyr::row_number())
#'
#'   overlapped <-
#'     query_gff %>%
#'     dplyr::rowwise() %>%
#'     dplyr::group_map(~ overlapped_feature(ordered_source_gff, .x$seqid, .x$start, .x$end)) %>%
#'     dplyr::bind_rows() %>%
#'     dplyr::distinct() %>%
#'     dplyr::select(-rn)
#'   if(include_query) {
#'     dplyr::full_join(overlapped, query_gff)
#'   } else {
#'     dplyr::anti_join(overlapped, query_gff)
#'   }
#' }
#'
#' add_upstream_feature <- function(gene_gff, row_id, bp) {
#'   new_row <- gene_gff[row_id,]
#'   if(new_row$strand == "+") {
#'     new_row$start <- new_row$start - bp
#'   } else {
#'     new_row$end <- new_row$end + bp
#'   }
#'   gene_gff %>%
#'     dplyr::bind_rows(new_row)
#' }
#'
#'
#' simple_gff_to_lineranges <- function(gff) {
#'   plot_df <-
#'     gff %>%
#'     dplyr::filter(type != "chromosome") %>%
#'     dplyr::filter(type != "protein") %>%
#'     dplyr::filter(type != "CDS") %>%
#'     gff_separate_attr() %>%
#'     dplyr::mutate(ID = ifelse(is.na(ID), Parent, ID)) %>%
#'     dplyr::mutate(gene_transcript = ifelse(type == "gene", "gene", "transcript"))
#'
#'   dplyr::filter(plot_df, type %in% c("gene", "mRNA")) %>%
#'     ggplot(aes(y = ID, group = attributes)) +
#'     geom_linerange(aes(xmin = start, xmax = end),
#'                    position = position_dodge2(width = .3)) +
#'     geom_linerange(
#'       data = dplyr::filter(plot_df, type == "exon"),
#'       aes(xmin = start, xmax = end), size = ggplot2::rel(2)) +
#'     geom_linerange(
#'       data = dplyr::filter(plot_df, !(type %in% c("gene", "mRNA", "exon"))),
#'       aes(xmin = start, xmax = end, color = type), size = ggplot2::rel(2)) +
#'     facet_grid(rows = vars(gene_transcript), scales = "free_y")
#' }
#'
#'
#' bstr2df <- function(bstr) {
#'   nm <- names(bstr)
#'   bstr %>%
#'     stringr::str_extract_all(".") %>%
#'     purrr::map(~ tibble::tibble(value = .x)) %>%
#'     purrr::map2(nm, ~ dplyr::mutate(.x, name = .y, rn = dplyr::row_number())) %>%
#'     dplyr::bind_rows()
#' }
#'
#' diff <- function(x) {
#'   x - dplyr::lag(x, default = FALSE)
#' }
#'
#' f <- function(li, maximum) {
#'   if(length(li$start) != length(li$end)) {
#'     li$end <- c(li$end, maximum)
#'   }
#'   li
#' }
#'
#' bstr2plot_df <- function(bstr) {
#'   df <- bstr %>% bstr2df()
#'   plot_df <-
#'     tidyr::pivot_wider(df) %>%
#'     {.[-1]} %>%
#'     {purrr::map_df(.x = ., function(x) .[[1]] == x)} %>%
#'     purrr::map_df(diff) %>%
#'     purrr::map(~ list(start =  which(.x == 1L), end = which(.x == -1))) %>%
#'     purrr::map(f, maximum = max(df$rn)) %>%
#'     purrr::imap_dfr(~ tibble::tibble(start = .x$start, end = .x$end, name = .y))
#'   plot_df %>%
#'     dplyr::mutate(name = forcats::fct_inorder(name))
#' }
#'
#' plot_bstr_linerange <- function(bstr) {
#'   bstr2plot_df(bstr) %>%
#'   dplyr::group_by(name) %>%
#'   dplyr::mutate(rn = dplyr::row_number()) %>%
#'   dplyr::ungroup() %>%
#'   ggplot() +
#'   geom_linerange(aes(xmin = start, xmax = end, y = name, group = rn))
#' }
#'
#' # temp %>%
#' #   bstr2df() %>%
#' #   dplyr::mutate(name = forcats::fct_inorder(name)) %>%
#' #   dplyr::filter(value != "-") %>%
#' #   ggplot(aes(rn, name)) +
#' #   geom_point(aes(color = value)) +
#' #   scale_color_discrete(l = 70)
#'
#'
#' mf <-
#'   function(v){
#'     if(!all(is.character(v))) stop("input is not character vector.")
#'     v_table <- table(v)
#'     tibble::tibble(
#'       tag = names(v_table),
#'       num = as.integer(v_table)
#'     )
#'   }
#'
#' #' Summarize GFF3
#' #' @param gff_df gff_df
#' #' @export
#' gff_summarise <-
#'   function(gff_df){
#'     list(
#'       seqid = mf(gff_df$seqid),
#'       source = mf(gff_df$source),
#'       type = mf(gff_df$type),
#'       score = mf(gff_df$score),
#'       strand = mf(gff_df$strand),
#'       phase = mf(gff_df$phase)
#'     )
#'   }
#'
#' #' Summarize GFF type column by seqid column
#' #' @param gff_df gff_df
#' #' @export
#' gff_summarise_type_by_seqid <-
#'   function(gff_df){
#'     . <- NULL
#'
#'     gff_df %>%
#'       split(.$seqid) %>%
#'       purrr::map(~ mf(.x[["type"]])) %>%
#'       purrr::imap(~ purrr::set_names(.x, c("tag", .y))) %>%
#'       purrr::reduce(dplyr::full_join, by = "tag") %>%
#'       purrr::map_df(tidyr::replace_na, 0L) %>%
#'       {dplyr::mutate(., All = dplyr::select(., -1) %>% rowSums())}
#'   }
