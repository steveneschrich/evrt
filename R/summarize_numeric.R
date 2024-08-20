#' Numeric data type
#'
#' @description A numeric type is a question involving a numeric output. These
#' can be options that can be treated as numeric if on a likert
#' scale, but can also be general numeric values.
#'
#' @details With a numeric variable, the summarizations are mean/median, etc
#'
#'
#' Possibilities include
#' - [summarize_numeric()]: Create a summarized table of the question(s)
#' - [plot_numeric()]: Create a figure of the numeric responses
#' - [gvsummary_numeric()]: Create a flextable and plot of the numeric responses.
#'
#' @name numeric
NULL




#' Summarize numeric variables
#'
#' @param x A data frame to summarize
#' @param vars Specific variables to summarize (default is all).
#' @param by Variable to stratify summary by.
#' @param dictionary The data dictionary for the instrument (optional)
#' @param header (default: "**Characteristic**") The summary table header
#' @param summary A summary measure (default = "mean")
#' @param include_overall Include a column of overall summary. Note this only
#'   works (and makes sense) when the `by` variable is used.
#' @param output A string representing the desired output. Choices include:
#'  - "gtsummary" A [gtsummary::gtsummary] object.
#'  - "flextable" A [flextable::flextable] table with formatting applied to a gtsummary table.
#'  - "rmarkdown" Raw flextable output for use in rmarkdown (see [flextable::flextable_to_rmd()]
#'    for details). This is most suitable for running summarize in a loop or as list component.
#'
#' @return A table object representing the numeric
#'  breakdown of `vars` from `x`.
#
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_numeric(iris, vars=c("Petal.Length","Petal.Width"))
#' }
summarize_numeric <- function(x,
                              vars = colnames(x),
                              by=NULL,
                              dictionary = NULL,
                              header = "**Characteristic**", summary="mean",
                              include_overall = TRUE,
                              output = c("gtsummary","flextable", "rmarkdown")) {

  output <- match.arg(output)
  # Get factor levels and convert everything to numeric (if needed).
  factor_levels <- get_factor_levels(x, vars, dictionary)

  #if ( is.null(dictionary) ) {
  #  x <- x |>
  #    dplyr::mutate(dplyr::across(
  #      dplyr::all_of(vars),
  #      \(.x){
  #        as.numeric(.x)
  #      }
  #    ))
  #}
  tbl <- gtsummary::tbl_summary(
    x,
    include=dplyr::all_of(vars),
    by= dplyr::all_of(by),
    type = list(everything() ~ "continuous"),
    statistic = list(everything() ~ ifelse(
      summary=="mean",
      "{mean} ({sd})",
      "{median} ({p25}, {p75})"
    )),
    digits = list(everything() ~ ifelse(
      summary=="mean", c(2,0), c(0,0))
    )) |>
    gtsummary::modify_header(label ~ header) |>
    gtsummary::modify_footnote(label~glue::glue(
      "Levels: {paste(names(factor_levels),factor_levels, sep='=', collapse=', ')}"
    ))

  # Add overall if needed
  if ( !is.null(by) && include_overall )
    tbl <- gtsummary::add_overall(tbl, last = TRUE, col_label="**Total** N = {N}")

  # Decide on output: gtsummary, flextable or rmarkdown.
  if ( output == "flextable" || output == "rmarkdown") {
    tbl <- gtsummary::as_flex_table(tbl) |>
      evrt::style_numeric_flextable()
    if ( output == "rmarkdown" )
      tbl <- flextable::flextable_to_rmd(tbl)
  }

  tbl
}

#' @describeIn summarize_numeric Flextable output of summarized numeric table
#' @export
summarize_numeric_as_flextable <- function(...) {
  summarize_numeric(..., output = "flextable")
}
#' @describeIn summarize_numeric gtsummary output of summarized numeric table
#' @export
summarize_numeric_as_gtsummary <- function(...) {
  summarize_numeric(..., output = "gtsummary")
}
#' @describeIn summarize_numeric rmarkdown output of summarized numeric table
#' @export
summarize_numeric_as_rmarkdown <- function(...) {
  summarize_numeric(..., output = "rmarkdown")
}


#' Style a numeric summary as a flextable
#'
#' @description Given a numeric summary, style the summary as a
#' [flextable::flextable()].
#'
#' @export
style_numeric_flextable <- function(...) {
  style_categorical_flextable(...)
}
