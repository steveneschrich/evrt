#' Create a basic summary table
#'
#' @description The simplest summary table would be evaluating all variables as categorical. This
#' function generates this summary to remain consistent with more complex summary styles.
#'
#' @param x
#' @param var
#' @param header
#' @param by Variable to stratify summary by.
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
summarize_basic <- function(x, var, header = "**Characteristic**", by=NULL) {
  gtsummary::tbl_summary(x,
                         # Include only selected variables
                         include=tidyselect::all_of(var),
                         # Force types to categorical - sometimes it is interpreted differently
                         type=list(tidyselect::everything() ~ "categorical"),
                         # Use the stratification variable.
                         by=by
   ) %>%
    # Change header to include header text
    gtsummary::modify_header(update = list(label ~ header))

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
  gtsummary::as_flex_table(x) %>%
    flextable::width(j=1,width=c(4)) %>%
    style_flextable()
}
