#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
followup_flextable <- function(x, ...) {
  summarize_followup(x, ...) |>
    style_followup_as_flextable()
}

#' Create a categorical table with followup comments
#'
#' @param x Labelled data frame containing all data in survey
#' @param vars Variable to include as frequency table (character)
#' @param followup Variable to include as follow-up field (character)
#' @param header
#' @param group_headers A vector of strings of header names for the questions (default
#'  QUESTION) and followup (default FOLLOWUP).
#' @param followup_statistic A statistic (see [gtsummary::tbl_summary()]). Default
#'   is `"{n}"`, meaning include counts.
#' @param include_overall Include a column of overall summary (no categories)
#' @param by Variable to stratify summary by.
#'
#' @return A [gtsummary::tbl_summary()] representing the frequency of `var`, with
#'  any additional free text responses in `followup` listed below the table.
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
summarize_followup <- function(x, vars, followup, header="**Characteristic**",
                               group_headers = c("QUESTION","FOLLOWUP"),
                               followup_statistic = "{n}",
                               include_overall = FALSE,
                               by = NULL) {

  # If embedded lists are present, expand them here.
  cattable <- dplyr::select(x, dplyr::all_of(c(vars,by))) |>
    expand_embedded_list(by=by)


  cattable<-cattable |>
    gtsummary::tbl_summary(type=list(gtsummary::everything() ~ "categorical"), by =  by)

  ftable <- dplyr::select(x, dplyr::all_of(c(followup, by))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(.x){dplyr::na_if(.x,"")})) |>
    gtsummary::tbl_summary(
      statistic = list(gtsummary::all_categorical() ~ followup_statistic),
      missing = "no",
      by = by
    )

  if (!is.null(by) && include_overall) {
    cattable <- gtsummary::add_overall(cattable, last = TRUE, col_label="**Total** N = {N}")
    ftable <- gtsummary::add_overall(ftable, last = TRUE, col_label="**Total** N = {N}")
  }

  tbl <- gtsummary::tbl_stack(list(cattable, ftable), quiet = TRUE,
                       group_header = group_headers) |>
    gtsummary::modify_header(update = list(label ~ header, groupname_col ~ "")) |>
    gtsummary::modify_footnote(update = gtsummary::everything() ~ NA)

  tbl

}



#' Convert followup table to flextable output
#'
#' @param x A gtsummary table
#'
#' @return A flextable object representing x.
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
style_followup_as_flextable <- function(x) {
  x |>
    gtsummary::as_flex_table() |>
    flextable::set_table_properties(layout="autofit", width=1) |>
    #flextable::width(j=1:6,width=c(1,3,0.75,0.75,0.75,0.75)) %>%
    style_flextable()
}
