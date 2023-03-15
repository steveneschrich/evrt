#' Choice Followup data type
#'
#' @description A choice followup type is a "select all that apply" question and a "Other, please
#' specify" followup-type question. The choice translates into multiple responses for each participant
#' (see [choice]). The result is a summary choice table [summarize_choice()] and a comments table
#' [summarize_comments()] combined.
#'
#' @details When counting choice types of variables, one can
#'  include the number of participants that responded with a particular choice and show
#'  all possible choices. See the [choice] page for more details on this type. Comments have
#'  their own options to consider (see [comments]). This type combines the two.
#'
#' Possibilities include
#' - summarize_choice_followup: Create a summarized table of the choices and a followup question.
#'
#' @name choice_followup
NULL


#' Summarize choice variable and followup variable to a table
#'
#' @param x A data frame
#' @param vars A list of variables (currently only one) to select. Note that as a
#'  choice variable, this corresponds to a choice set "stem", or the text preceding
#'  the `___N` in the name.
#' @param followup A column to use as the follow up question/response (comment).
#' @param followup_statistic ("{n}") A statistic to use in the followup table.
#' @param by An optional stratification of the choices when summarizing.
#' @param include_overall (TRUE) Include a column of overall counts. Only useful when
#'  stratifying (`by`).
#' @param output A string representing the desired output. Choices include:
#'  - "gtsummary" A [gtsummary::gtsummary] object.
#'  - "flextable" A [flextable::flextable] table with formatting applied to a gtsummary table.
#'  - "rmarkdown" Raw flextable output for use in rmarkdown (see [flextable::flextable_to_rmd()]
#'    for details). This is most suitable for running summarize in a loop or as list component.
#'
#' @return A summary table object which summarizes the counts of observations in
#' the choice variable(s). Results are in the format described in `output`.
#' @export
#'
summarize_choice_followup <- function(x, vars = NULL, followup, followup_statistic = "{n}",
                                      by = NULL, include_overall = TRUE,
                             output = c("gtsummary","flextable", "rmarkdown")) {
  stopifnot("Only one variable is supported for summarize_choice." = length(vars) == 1)
  output <- match.arg(output)

  overall_table <- summarize_choice(x, vars = vars, by = by, include_overall=include_overall,
                                   output = "gtsummary")
  comment_table <- summarize_comments(x, vars = followup, by = by,
                                      statistic = followup_statistic, include_overall=include_overall,
                                      output = "gtsummary"
  )

  tbl <- gtsummary::tbl_stack(list(overall_table, comment_table), quiet = TRUE)

  # Decide on output: gtsummary, flextable or rmarkdown.
  if ( output == "flextable" || output == "rmarkdown") {
    tbl <- gtsummary::as_flex_table(tbl) |>
      evrt::style_flextable()
    if ( output == "rmarkdown" )
      tbl <- flextable::flextable_to_rmd(tbl)
  }

  tbl
}


