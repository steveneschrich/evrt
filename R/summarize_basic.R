#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
basic_flextable <- function(x, ...) {
  summarize_basic(x, ...) |>
    style_basic_as_flextable()
}


#' Create a basic summary table
#'
#' @description The simplest summary table would be evaluating all variables as categorical. This
#' function generates this summary to remain consistent with more complex summary styles.
#'
#' @param x
#' @param vars
#' @param header
#' @param by Variable to stratify summary by.
#' @param include_overall Include a column of overall summary (no categories)
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
summarize_basic <- function(x, vars, header = "**Characteristic**", by=NULL,
                            include_overall = FALSE) {


  x <- x |>
    dplyr::select(dplyr::all_of(c(vars, by))) |>
    expand_embedded_list(by=by)

  tbl <- gtsummary::tbl_summary(x,
                         # Include only selected variables
                         include=tidyselect::all_of(vars),
                         # Force types to categorical - sometimes it is interpreted differently
                         type=list(tidyselect::everything() ~ "categorical"),
                         # Use the stratification variable.
                         by=by
   ) |>
    # Change header to include header text
    gtsummary::modify_header(update = list(label ~ header))

  # Add overall if needed
  if ( !is.null(by) && include_overall )
    tbl <- gtsummary::add_overall(tbl, last = TRUE, col_label="**Total** N = {N}")

  tbl
}



#' Convert basic summary table to flextable output
#'
#' @description The basic summary table can be converted to flextable output, with
#' a few simple styling options.
#'
#' @param x A summary table
#'
#' @return A flextable
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
style_basic_as_flextable <- function(x) {
  gtsummary::as_flex_table(x) |>
    flextable::autofit() |>
    flextable::set_table_properties(layout="autofit", width=1) |>
    style_flextable()
}
