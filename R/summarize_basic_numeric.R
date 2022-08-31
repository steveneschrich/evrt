#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
numeric_flextable <- function(x, ...) {
  summarize_basic_numeric(x, ...) |>
    style_basic_as_flextable()
}

#' Basic numerical summary table
#'
#' @param x
#' @param vars
#' @param dictionary The data dictionary for the instrument
#' @param header
#' @param summary
#'
#' @return
#' @export
#'
#' @examples
summarize_basic_numeric <- function(x, vars, dictionary,
                                          header = "**Characteristic**", summary="mean",
                                          by=NULL) {
  factor_levels <- get_factor_levels(dictionary, vars)
  checkmate::assert_vector(factor_levels, len=1)

  gtsummary::tbl_summary(x, include=dplyr::all_of(vars), by= by,
                         type = list(everything() ~ "continuous"),
                         statistic = list(everything() ~ ifelse(summary=="mean", "{mean} ({sd})", "{median} ({p25}, {p75})")),
                         digits = list(everything() ~ ifelse(summary=="mean", c(2,0), c(0,0)))) %>%
    gtsummary::modify_header(update = list(label ~ header)) %>%
    gtsummary::modify_footnote(label~glue::glue("Levels: {factor_levels}"))
}
