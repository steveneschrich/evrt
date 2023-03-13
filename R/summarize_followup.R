#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
flextable_followup <- function(x,  vars,
                               followup,
                               header = "",
                               by=NULL,
                               group_headers =  c("",""),
                               followup_statistic = "{n}",
                               include_overall = FALSE ) {

  summarize_followup(x, vars, followup=followup, header=header,
                     group_headers = group_headers,
                     followup_statistic = followup_statistic,
                     include_overall=include_overall,
                     by = by) |>
    as_flextable() |>
    style_followup_flextable()
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
    # Replace the NA values with "" to cleaner reporting.
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        \(.x){
          labelled::recode_if(.x,.x == "", NA)
        }
      )
    ) |>
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



#' Style flextable as a followup table
#'
#' @description Apply consistent styling to a follow up flextable
#'  to fit the typical contents of the table.
#'
#' @param x A flextable
#'
#' @return A flextable with specific styling applied
#' @export
#'
#' @examples
#' \dontrun{
#' gtsummary::tbl_summary(iris) |>
#'   as_flextable() |>
#'   style_followup_flextable()
#' }
style_followup_flextable <- function(x) {
  x |>
    flextable::set_table_properties(layout="autofit", width=1) |>
    #flextable::width(j=1:6,width=c(1,3,0.75,0.75,0.75,0.75)) %>%
    style_flextable()
}
