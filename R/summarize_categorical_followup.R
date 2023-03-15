#' Categorical Followup data type
#'
#' @description A categorical followup type is a question involving one response out of
#' a set of options. These options can be treated as numeric if on a likert
#' scale, but generally are categorical. The followup is a question like "Other, please specify"
#'
#'
#' @details With a categorical variable, the summarizations are frequency
#' tables by category level.
#'
#' Possibilities include
#' - summarize_categorical_followup: Create a summarized table of the question(s) and append the
#' followup responses.
#' - plot_categorical_followup: Create a figure of the categorical response frequencies (no
#' followup responses).
#' - gvsummary_categorical_followup: Create a flextable and plot of the categorical responses.
#'
#' @name categorical_followup
NULL


#' Create a categorical table with followup comments
#'
#' @param x Labelled data frame containing all data in survey
#' @param vars (default: dplyr::everything) Columns of the data frame
#'    that contain categorical fields
#' @param by Variable to stratify summary by.
#' @param followup A column to use as the follow up question/response (comment).
#' @param followup_statistic A statistic (see [gtsummary::tbl_summary()]). Default
#'   is `"{n}"`, meaning include counts.
#' @param header The overall header for the table
#' @param group_headers A vector of strings of header names for the questions (default
#'  QUESTION) and followup (default FOLLOWUP).
#' @param include_overall (TRUE) Include a column of overall counts. Only useful when
#'  stratifying (`by`).
#' @param output A string representing the desired output. Choices include:
#'  - "gtsummary" A [gtsummary::gtsummary] object.
#'  - "flextable" A [flextable::flextable] table with formatting applied to a gtsummary table.
#'  - "rmarkdown" Raw flextable output for use in rmarkdown (see [flextable::flextable_to_rmd()]
#'    for details). This is most suitable for running summarize in a loop or as list component.
#'
#' @return A table representing the frequency of `vars`, with
#'  any additional free text responses in `followup` listed below the table.
#'
#' @export
#'
summarize_categorical_followup <- function(x, vars, by = NULL,
                                           followup,followup_statistic = "{n}",
                                           header="**Characteristic**",
                                           group_headers = c("",""),

                               include_overall = FALSE,
                               output = c("gtsummary","flextable", "rmarkdown")
                               ) {

  output <- match.arg(output)
  cattable<-summarize_categorical(
    x,
    vars = vars,
    include_overall=include_overall,
    by = by,
    output = "gtsummary",
    header = header
  )

  ftable <- summarize_comments(
    x,
    vars = followup,
    include_counts = TRUE,
    by = by,
    include_overall = include_overall,
    output = "gtsummary",
    statistic = followup_statistic
  )

  tbl <- gtsummary::tbl_stack(list(cattable, ftable), quiet = TRUE,
                       group_header = group_headers) |>
    # Empty the groupname_col (header) since these aren't groups, just an
    # extension of the same thing.
    gtsummary::modify_header(update = list(groupname_col ~ ""))

  # Decide on output: gtsummary, flextable or rmarkdown.
  if ( output == "flextable" || output == "rmarkdown") {
    tbl <- gtsummary::as_flex_table(tbl) |>
      evrt::style_categorical_followup_flextable()
    if ( output == "rmarkdown" )
      tbl <- flextable::flextable_to_rmd(tbl)
  }

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
style_categorical_followup_flextable <- function(x) {
  x |>
    flextable::set_table_properties(layout="autofit", width=1) |>
    #flextable::width(j=1:6,width=c(1,3,0.75,0.75,0.75,0.75)) %>%
    style_flextable()
}
