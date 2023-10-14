#' Comments data type
#'
#' @description Comments are free-text responses that are summarized into
#'  a table of non-NA rows.
#' @name comments
NULL


#' Create a comment table
#'
#' @description Extract variables from the table and format the non-empty
#'   strings within these variables as a summary table.
#'
#' @details This function combines (potentially) multiple columns of a
#' data frame into a single output summary table. This table has the
#' question name (or column name if there is no [labelled::var_label()])
#' followed by all non-empty values, one per line.
#'
#' This is most useful as a way to summarize a question (or questions)
#' that have free-text answers. This function just shows the responses.
#'
#' @param x The data frame to summarize
#' @param vars (default: dplyr::everything) Columns of the data frame
#'    that contain comments
#' @param header (default: "**Comments**") The summary table header
#' @param output A string representing the desired output. Choices include:
#'  - "gtsummary" A [gtsummary::gtsummary] object.
#'  - "flextable" A [flextable::flextable] table with formatting applied to a gtsummary table.
#'  - "rmarkdown" Raw flextable output for use in rmarkdown (see [flextable::flextable_to_rmd()]
#'    for details). This is most suitable for running summarize in a loop or as list component.

#' @return A [gtsummary::tbl_summary()] object representing the non-empty
#'  fields of `vars` listed.
#'
#' @seealso style_comments_as_flextable()
#' @export
#'
#' @examples
#' \dontrun{
#' tibble::tibble("This is a question"=c("comment 1","comment 2", NA,"comment 3")) |>
#'   summarize_comments()
#' }
summarize_comments <- function(x,
                               vars = dplyr::everything(x),
                               header = "",
                               include_counts = FALSE,
                               by = NULL,
                               include_overall = FALSE,
                               output = c("gtsummary","flextable", "rmarkdown"),
                               statistic = ""
                               ) {
  output <- match.arg(output)

  if ( include_counts )
    statistic = "{n}"

  tbl <- x |>
    gtsummary::tbl_summary(
      include = dplyr::all_of(vars),
      statistic=list(gtsummary::all_categorical() ~ statistic),
      missing = "no",
      by = dplyr::all_of(by)
    ) |>
    gtsummary::modify_header(update = list(label ~ header)) |>
    gtsummary::modify_footnote(update = gtsummary::everything() ~ NA)

  if ( !is.null(by) && include_overall )
    tbl <- gtsummary::add_overall(tbl, last = TRUE, col_label = "**Total** N = {N}")


  # Decide on output: gtsummary, flextable or rmarkdown.
  if ( output == "flextable" || output == "rmarkdown") {
    tbl <- gtsummary::as_flex_table(tbl) |>
      evrt::style_comments_as_flextable()
    if ( output == "rmarkdown" )
      tbl <- flextable::flextable_to_rmd(tbl)
  }

  tbl
}


#' @describeIn summarize_comments Flextable output of summarized comments table
#' @export
summarize_comments_as_flextable <- function(...) {
  summarize_comments(..., output = "flextable")
}
#' @describeIn summarize_comments gtsummary output of summarized comments table
#' @export
summarize_comments_as_gtsummary <- function(...) {
  summarize_comments(..., output = "gtsummary")
}
#' @describeIn summarize_comments rmarkdown output of summarized comments table
#' @export
summarize_comments_as_rmarkdown <- function(...) {
  summarize_comments(..., output = "rmarkdown")
}
#' Style flextable with sensible defaults
#'
#' @description Style a [flextable::flextable()] comments
#'  object, applying default styling appropriate for comments.
#'
#'
#' @param x A [flextable::flextable()] object for styling.
#' @param widths (Default: c(5,1)): The relative widths of the
#'  two columns representing comments (usually Question, Response).
#'
#' @return A [flextable::flextable()] object representing the comment
#'  table `x`, appropriately styled.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  tibble::tibble("This is a question"=c("comment 1","comment 2")) |>
#'   summarize_comments() |>
#'   style_comments_as_flextable()
#'
#' }
style_comments_as_flextable <- function(x, widths = c(5,1)) {

  x|>
    # Sensible defaults or overridden
    flextable::width(j=1:2,width=widths) |>
    style_flextable()
}
