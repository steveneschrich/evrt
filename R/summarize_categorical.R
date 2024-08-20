#' Categorical data type
#'
#' @description A categorical type is a question involving one response out of
#' a set of options. These options can be treated as numeric if on a likert
#' scale, but generally are categorical.
#'
#' @details With a categorical variable, the summarizations are frequency
#' tables by category level.
#'
#' Possibilities include
#' - summarize_categorical: Create a summarized table of the question(s)
#' - plot_categorical: Create a figure of the categorical response frequencies.
#' - gvsummary_categorical: Create a flextable and plot of the categorical responses.
#'
#' @name categorical
NULL




#' Create a basic categorical variable breakdown table
#'
#' @description Extract variables from the table and format the variables
#'   by value within a summary table.
#'
#' @details This function combines (potentially) multiple columns of a
#' data frame into a single output summary table. This table has the
#' variable name (or column name if there is no [labelled::var_label()])
#' followed by a summarization of levels for this variable. If your
#' variable is numeric, please see [summarize_numeric()].
#'
#' @param x The data frame to summarize
#' @param vars (default: dplyr::everything) Columns of the data frame
#'    that contain categorical fields
#' @param by Variable to stratify summary by.
#' @param header (default: "**Characteristic**") The summary table header
#' @param include_overall Include a column of overall summary. Note this only
#'   works (and makes sense) when the `by` variable is used.
#' @param output A string representing the desired output. Choices include:
#'  - "gtsummary" A [gtsummary::gtsummary] object.
#'  - "flextable" A [flextable::flextable] table with formatting applied to a gtsummary table.
#'  - "rmarkdown" Raw flextable output for use in rmarkdown (see [flextable::flextable_to_rmd()]
#'    for details). This is most suitable for running summarize in a loop or as list component.
#'
#' @return A [gtsummary::tbl_summary()] object representing the categorical
#'  breakdown of `vars` from `x`.
#'
#' @seealso style_categorical_flextable()
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_categorical(ToothGrowth, vars =c("dose"),by="supp")
#' }
#'
summarize_categorical<- function(x, vars,
                                 header = "", by=NULL,
                            include_overall = FALSE,
                            output = c("gtsummary","flextable", "rmarkdown")) {

  output <- match.arg(output)
 # x <- x |>
#    dplyr::select(dplyr::all_of(c(vars, by))) |>
#    expand_embedded_list(by=by)

  tbl <- gtsummary::tbl_summary(x,
                         # Include only selected variables
                         include=dplyr::all_of(vars),
                         # Force types to categorical - sometimes it is interpreted differently
                         type=list(tidyselect::everything() ~ "categorical"),
                         # Use the stratification variable.
                         by=dplyr::all_of(by)
   ) |>
    # Change header to include header text
    gtsummary::modify_header(label ~ header)

  # Add overall if needed
  if ( !is.null(by) && include_overall )
    tbl <- gtsummary::add_overall(tbl, last = TRUE, col_label="**Total** N = {N}")

  # Decide on output: gtsummary, flextable or rmarkdown.
  if ( output == "flextable" || output == "rmarkdown") {
    tbl <- gtsummary::as_flex_table(tbl) |>
      evrt::style_categorical_flextable()
    if ( output == "rmarkdown" )
      tbl <- flextable::flextable_to_rmd(tbl)
  }

  tbl
}

#' @describeIn summarize_categorical Flextable output of summarized categorical table
#' @export
summarize_categorical_as_flextable <- function(...) {
  summarize_categorical(..., output = "flextable")
}
#' @describeIn summarize_categorical gtsummary output of summarized categorical table
#' @export
summarize_categorical_as_gtsummary <- function(...) {
  summarize_categorical(..., output = "gtsummary")
}
#' @describeIn summarize_categorical rmarkdown output of summarized categorical table
#' @export
summarize_categorical_as_rmarkdown <- function(...) {
  summarize_categorical(..., output = "rmarkdown")
}

#' Apply style to catgorical flextable
#'
#' @description Apply a consistent style to a [flextable::flextable()]
#'  object appropriate for categorical output.
#'
#' @details A [flextable::flextable()] is useful visualiation, particularly within
#'  a MS-Word document or HTML output. This function does appropriate
#'  styling of a categorical flextable. Along the way, it applies some styling to the table
#'  that has generally been helpful for visualization.
#'
#' @param x A [flextable::flextable()] object for styling.
#'
#' @return A [flextable::flextable()] object representing the categorical
#'  table `x`, appropriately styled.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   summarize_categorical(ToothGrowth, vars=c("dose"), by="supp") |>
#'   as_flextable() |>
#'   style_categorical_flextable()
#'
#' }
style_categorical_flextable <- function(x) {
  x |>
    flextable::autofit() |>
    flextable::set_table_properties(layout="autofit", width=1) |>
    style_flextable()
}
