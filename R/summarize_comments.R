#' Generate a flextable of comments in a data frame
#'
#' @param x A data frame
#' @param vars (default: dplyr::everything()) Variables that
#'    have comments
#' @param header (default: "**Comments**") Header text of the
#'    flextable
#'
#' @return A comment table formatted as a [flextable::flextable()]
#' @export
#'
#' @examples
#' \dontrun{
#' tibble::tibble("This is a question"=c("comment 1","comment 2")) |>
#'   flextable_comments()
#' }
flextable_comments <- function(x, vars = dplyr::everything(x),
                               header = "**Comments**") {
  summarize_comments(x,vars = vars, header = header) |>
    style_comments_as_flextable()
}


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
#'
#' @return A [gtsummary::tbl_summary()] object representing the non-empty
#'  fields of `vars` listed.
#'
#' @seealso style_comments_as_flextable()
#' @export
#'
#' @examples
#' \dontrun{
#' tibble::tibble("This is a question"=c("comment 1","comment 2")) |>
#'   summarize_comments()
#' }
summarize_comments <- function(x,
                               vars = dplyr::everything(x),
                               header = "**Comments**") {
   select_and_na(x, vars) |>
    gtsummary::tbl_summary(
      statistic=list(gtsummary::all_categorical() ~ ""),
      missing = "no"
    ) |>
    gtsummary::modify_header(update = list(label ~ header)) |>
    gtsummary::modify_footnote(update = gtsummary::everything() ~ NA)
}


#' Convert comment table to styled flextable
#'
#' @description Convert a comment table into a [flextable::flextable()]
#'  object, applying default styling appropriate for comments.
#'
#' @details A [gtsummary::tbl_summary()] table summarizes a data frame
#'  full of information. However, we often would like to visualize it
#'  as a nicely-formatted [flextable::flextable()], particularly within
#'  a MS-Word document or HTML output. This function does this conversion
#'  to a flextable. Along the way, it applies some styling to the table
#'  that has generally been helpful for visualization.
#'
#' @param x A [gtsummary::tbl_summary()] object for converting.
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

    # Convert to flextable
    gtsummary::as_flex_table(x) |>
    # Sensible defaults or overridden
    flextable::width(j=1:2,width=widths) |>
    style_flextable()
}
