#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
flextable_numeric <- function(x, ...) {
  summarize_numeric(x, ...) |>
    as_flextable() |>
    style_numeric_flextable()
}




#' Summarize numeric variables
#'
#' @param x A data frame to summarize
#' @param vars Specific variables to summarize (default is all).
#' @param dictionary The data dictionary for the instrument (optional)
#' @param header
#' @param summary
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_numeric(iris, vars=c("Petal.Length","Petal.Width"))
#' }
summarize_numeric <- function(x,
                              vars = colnames(x), dictionary = NULL,
                              header = "**Characteristic**", summary="mean",
                              by=NULL) {

  # Get factor levels and convert everything to numeric (if needed).
  factor_levels <- get_factor_levels(x, vars, dictionary)
  if ( is.null(dictionary) ) {
    x <- x |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(vars),
        \(.x){
          as.numeric(.x)
        }
      ))
  }
  gtsummary::tbl_summary(
    x,
    include=dplyr::all_of(vars),
    by= by,
    type = list(everything() ~ "continuous"),
    statistic = list(everything() ~ ifelse(
      summary=="mean",
      "{mean} ({sd})",
      "{median} ({p25}, {p75})"
    )),
    digits = list(everything() ~ ifelse(
      summary=="mean", c(2,0), c(0,0))
    )) |>
    gtsummary::modify_header(update = list(label ~ header)) |>
    gtsummary::modify_footnote(label~glue::glue(
      "Levels: {paste(names(factor_levels),factor_levels, sep='=', collapse=', ')}"
    ))
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
