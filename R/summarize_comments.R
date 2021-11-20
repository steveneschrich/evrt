#' Create a comment table
#'
#' @param x
#' @param var
#'
#' @return
#' @export
#'
#' @examples
summarize_comments <- function(x, var, header = "**Characteristic**") {
  dplyr::select(x, {{var}}) %>%
    dplyr::na_if("") %>%
    gtsummary::tbl_summary(statistic=list(gtsummary::all_categorical() ~ ""),
                           missing = "no") %>%
    gtsummary::modify_header(update = list(label ~ header)) %>%
    gtsummary::modify_footnote(update = gtsummary::everything() ~ NA)
}
#' Convert comment table to flextable output
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
style_comments_as_flextable <- function(x) {
  x %>%
    gtsummary::as_flex_table() %>%
    flextable::width(j=1:2,width=c(5,1)) %>%
    style_flextable()
}
