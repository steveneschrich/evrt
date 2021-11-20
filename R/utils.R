#' Wrap the text of a factor, recreating the factor.
#'
#' @param x A list of factor data
#' @param width Width to wrap text.
#'
#' @return
#' @export
#'
#' @examples
fct_wrap<-function(x, width=25) {
  checkmate::assert_factor(x)
  # To do this, we need to wrap the levels of the factor then wrap the strings and
  # reapply the levels.
  wrapped_levels <- stringr::str_wrap(levels(x), width)
  x<-stringr::str_wrap(x, width)
  x <- factor(x, levels=wrapped_levels)

  x
}



#' Extract variable labels as a tibble
#'
#' @description Variable labels can be set on tibble column names. This function
#' extracts them and formats them as a tibble for easy manipulation.
#'
#' @details Variable labels in the `labelled` package allow one to create labels that may be
#' question text or other longer-form text associated with a variable. The `labelled` package
#' provides a way to get at these, using the \code{\link[labelled]{var_label}} function. This
#' function returns a named list. However, within the tidyverse it's often easier to manipulate
#' the labels as a tibble. This function provides that.
#'
#' The returned tibble has two columns:
#' \itemize{
#' \item variable
#' \item label
#' }
#' @param x A tibble to extract labels from.
#'
#' @return A tibble representing the labels. Two columns are returned: `variable` and `label`.
#' @export
#'
#' @examples
#' extract_var_labels(labelled::set_variable_labels(tibble::tribble(~a,~b,1,2), a="First variable", b="Second variable"))
extract_var_labels<-function(x) {
  # Extract and save labels
  labelled::var_label(x) %>%
    unlist() %>%
    tibble::enframe(name = "variable", value = "label")
}
