#' Wrap the text of factor levels
#'
#' @description Rewrite a factor by wrapping the levels of the factor
#'  to a shorter width (per line). Will coerce a non-factor to factor
#'  first, then shorten the labels.
#'
#' @param x A list of factor data
#' @param width Width to wrap text.
#'
#' @return A list of factor data with the levels wrapped at `width`.
#' @export
#'
#' @examples
#' \dontrun{
#' fct_wrap(paste(iris$Species, "iris", sep=" "), width=2)
#' }
fct_wrap<-function(x, width=25) {

  if ( !is.factor(x) )
    x <- as.factor(x)

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
#' @param vars A list of variables to extract labels (can use [tidyselect::tidyselect()] syntax).
#' @return A tibble representing the labels. Two columns are returned: `variable` and `label`.
#' @export
#'
#' @examples
#' extract_var_labels(labelled::set_variable_labels(tibble::tribble(~a,~b,1,2), a="First variable", b="Second variable"))
extract_var_labels<-function(x, vars = dplyr::everything()) {

  # Subset and order variables (default is everything)
  dplyr::select(x, tidyselect::eval_select(expr = vars, data = x)) |>
    # Extract labels (for subset, in order)
    labelled::var_label(unlist=TRUE) |>
    # Turn into tibble.
    tibble::enframe(name = "variable", value = "label")
}



#' Reduce data frame and set empty strings to NA
#'
#' @param x A data frame
#' @param vars The variables to select
#'
#' @return
#' @export
#'
#' @examples
select_and_na <- function(x, vars) {
  dplyr::select(x, {{vars}}) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        \(.x) {
          labelled::recode_if(.x, .x == "", NA)
        }
      )
    )
}


#' Retrieve labels from data frame
#'
#' @description Retrieve variable labels from data frame columns,
#' using the colnames for labels that are not present.
#'
#' @details The [labelled::var_label()] function provides variable
#' labels within a data frame. However, if the variable label is
#' missing it returns NULL. This function provides a reasonable
#' default in the case of NULL, namely the column name itself.
#'
#' @param x A data frame, possibly with variable labels.
#' @param vars A list of variables to restrict label extraction to.
#'
#' @return A named list of variable labels. Names are the columns
#'  in the data frame. Values are the labels (or column names if
#'  labels are missing).
#'
#' @export
#'
#' @importFrom rlang %||%
#' @examples
#' \dontrun{
#' default_labels(ToothGrowth, vars=c("supp"))
#' }
default_labels <- function(x, vars = colnames(x)) {
  # Default values
  defaults <- stats::setNames(vars, vars)
  # Choose labels or default values.
  unlist(labelled::var_label(x)[vars]) %||% defaults
}

#' Convert input object to flextable
#'
#' @description This function is short for the [gtsummary::as_flextable()]
#'  function to provide cleaner code within the package.
#'
#' @param x An object to convert to flextable
#'
#' @return A [flextable::flextable()] object.
#' @export
#'
#' @examples
#' \dontrun{
#' as_flextable(gtsummary::tbl_summary(iris))
#' }
as_flextable <- function(x) {
  gtsummary::as_flex_table(x)
}


