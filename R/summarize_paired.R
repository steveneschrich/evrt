


#' Title
#'
#' @param data
#' @param variable
#' @param by
#' @param summarizer
#' @param ...
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
paired_calculation <- function(data, variable, by,summarizer, ...) {
  calculate_paired(data, variable, by=summarizer) %>%
    dplyr::mutate(summary =
                    stringr::str_glue("{gtsummary::style_sigfig(mean)} ({gtsummary::style_sigfig(sd)})")) %>%
    dplyr::select(.data$summary, .data$p.value)

}


#' Title
#'
#' @param x
#' @param vars
#' @param dictionary
#' @param header
#' @param summarizer
#'
#' @return
#' @export
#'
#' @examples
summarize_paired <- function(x, vars, dictionary, header = "**Characteristic**", summarizer=status ~ post - pre ) {
  factor_levels <- get_factor_levels(dictionary, vars)
  checkmate::assert_vector(factor_levels, len=1)

  tbl <- x %>%
    gtsummary::tbl_summary(include = dplyr::all_of(vars),
                           by=!!formula.tools::lhs(summarizer),
                           type = list(everything() ~ "continuous"),
                           statistic = list(everything() ~ "{mean} ({sd})"),
                           digits = list(everything() ~c(1,2))) %>%
    gtsummary::add_stat(fns = everything() ~ function(...) {
      paired_calculation(summarizer=summarizer, ...)
    }) %>%
    gtsummary::modify_header(list(p.value ~ "**p-value**",
                                  summary ~ "**Difference**")) %>%
    # TODO: Fix this so the same numbers are always there, with 3 digits
    gtsummary::modify_fmt_fun(list(p.value ~ gtsummary::style_pvalue)) %>%
    gtsummary::modify_footnote(update = list(
      summary ~ "Paired pre/post differences - Mean (SD).",
      p.value ~ "Wilcoxon rank sum test from paired pre/post assessments.",
      label ~ stringr::str_wrap(glue::glue("Levels: {factor_levels}"), width = 60)
    )) %>%
    gtsummary::modify_header(update = list(label ~ header))

  tbl
}

#' Convert comment table to flextable output
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
style_paired_as_flextable <- function(x) {
  x %>%
    gtsummary::as_flex_table() %>%
    flextable::width(j=1,width=c(2)) %>%
    style_flextable()
}


