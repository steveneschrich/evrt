
#' Calculate count of observations of a vector at a threshold or greater
#'
#' @param x A vector of factor values to count
#' @param threshold A threshold (factor value) to count at or above
#' @param na.rm (TRUE) Remove NA values from count
#'
#' @return A count of the number of elements at or above threshold
#' @export
#'
#' @examples
#' \dontrun{
#' count_at_or_above_threshold(iris$Species, "versicolor")
#' }
count_at_or_above_threshold <- function(x,threshold, na.rm=TRUE) {
  # x must be a factor
  checkmate::assert_true(is.factor(x))
  # threshold must be a level of x
  checkmate::assert_true(threshold %in% levels(x))

  if (na.rm) x <- na.omit(x)

  # Calculate frequency of all values
  tibble::tibble(
    count = forcats::fct_count(x) |>
      dplyr::mutate(factor_index = as.numeric(f)) |>
      dplyr::filter(factor_index >= match(threshold, levels(x))) |>
      dplyr::summarize(count = sum(n)) |>
      dplyr::pull("count"),
    n = length(x)
  )
}


#' Count the number of Yes values in a list
#'
#' @param x A list, assumed to be a factor variable with values `Yes` and `No`.
#'
#' @return A count of the number of values that are `Yes`
#' @export
#'
#' @examples
#' \dontrun{
#' count_yes(factor(c("Yes","No","Yes","Yes")))
#' }
#' # [1] 3
#' }
count_yes <- function(x, na.rm = TRUE) {
  checkmate::assert_factor(x, levels=c("Yes","No"))
  if (na.rm) x <- na.omit(x)

  tibble::tibble(
    count = forcats::fct_count(x) |> dplyr::filter(f=="Yes") |> dplyr::pull("n"),
    n = length(x)
  )

}

#' Count length of x
#'
#' @param x A data frame
#' @param vars A list of variables for counting (not used)
#'
#' @return A count of x
#' @export
#'
#' @examples
count_length <- function(x, vars) {
  tibble::tibble(
    count = length(x),
    n = length(x)
  )
}
#' Title
#'
#' @param x
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
count_variables <- function(x, vars) {
  x |>
    dplyr::summarize(dplyr::across(dplyr::all_of(vars), ~length(na.omit(.x)))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(vars), values_to="n", names_to="variable")
}
