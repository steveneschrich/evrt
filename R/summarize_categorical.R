#' Generate a flextable of categorical variables in a data frame
#'
#' @param x The data frame to summarize
#' @param vars (default: dplyr::everything) Columns of the data frame
#'    that contain categorical fields
#' @param header (default: "**Characteristic**") The summary table header
#' @param by Variable to stratify summary by.
#' @param include_overall Include a column of overall summary. Note this only
#'   works (and makes sense) when the `by` variable is used.
#'
#' @return A [gtsummary::tbl_summary()] object representing the categorical
#'  breakdown of `vars` from `x`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flextable_categorical(ToothGrowth, vars=c("dose"),
#'    by = "supp", include_overall=TRUE)
#' }
flextable_categorical <- function(x,
                                  vars,
                                  header = "**Characteristic**",
                                  by=NULL,
                                  include_overall = FALSE ) {
  summarize_categorical(x, vars=vars, header=header, by=by,
                        include_overall=include_overall) |>
    as_flextable() |>
    style_categorical_flextable()
}


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
#' @param header (default: "**Characteristic**") The summary table header
#' @param by Variable to stratify summary by.
#' @param include_overall Include a column of overall summary. Note this only
#'   works (and makes sense) when the `by` variable is used.
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
                                 header = "**Characteristic**", by=NULL,
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
