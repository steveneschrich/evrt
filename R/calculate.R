#' Calculate numerical summaries of tibble for plotting
#'
#' @description Calculate standard statistics (mean, sd, se, n) from a
#' list of variables in a tibble.
#'
#' @note At the moment, `by` is a bare word not a string.
#'
#' @param x Tibble to summarize
#' @param vars Variables to summarize
#' @param by
#'
#' @return
#' @export
#'
#' @examples
calculate_numerical_summaries <- function(x, vars, by=NULL) {
  tidy_data_for_calculation(x, vars, by) %>%
    # Then compute statistics on the variable
    dplyr::summarize(
      mean=mean(value),
      sd = sd (value),
      se = sd(value)/ sqrt(dplyr::n()),
      n = dplyr::n()
    ) %>%
    format_calculations_for_plotting(x, vars)

}

#' Title
#'
#' @param x
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
calculate_counts_factor <- function(x, threshold = NULL) {
  checkmate::assert_factor(x)
  checkmate::assert_true(threshold %in% levels(x))
  # index of threshold in the levels of the factor
  i <- stringr::str_which(levels(x), glue::glue("^{threshold}$"))
  # Enumerate levels that are at threshold or later in the levels list (for now).
  matching_levels <- levels(x)[i:nlevels(x)]
  # Count how many of x are in matching_levels
  length(which(x %in% matching_levels))
}
cc_yesno <- function(x) {
  checkmate::assert_factor(x, levels=c("Yes","No"))
  length(which(x %in% "Yes"))
}
#' Title
#'
#' @param x
#' @param vars
#' @param threshold
#' @param by
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @examples
calculate_counts_at_threshold <- function(x, vars, threshold = 0, by=NULL) {

  # Tidy data for summarization (variable, value, by_field (optional)) grouped
  # by variable and by.
  tidy_data_for_calculation(x, vars, by) %>%
    dplyr::summarize(
      count = calculate_counts_factor(.data$value, threshold),
      percent =  (count/dplyr::n()),
      n = dplyr::n()
    ) %>%
    # Format results appropriately for plotting
    format_calculations_for_plotting(x, vars=vars)

}

#' Tidy data for summarization
#'
#' @description Prior to summarization, the data must be tidied for easier
#' summarization operations.
#'
#' @details
#' Most of the tidyverse works better with tidy data. This extends to the notion
#' of long data (vs wide data). This function will transform a data table to
#' a series of entries:
#' \itemize{
#' \item variable
#' \item value
#' \item group_by_field
#' }
#'
#' After the transformation, the data is grouped by `variable` and `by` so that
#' summarization happens naturally via grouping.
#'
#' @note The `by` variable can be NULL, in which case grouping occurs just by
#' variable.
#'
#' @param x
#' @param vars
#' @param by
#'
#' @return
#'
#' @examples
tidy_data_for_calculation<- function(x, vars, by) {
  if (!is.null(by)) by <- rlang::ensym(by)

  # Extract vars from table
  dplyr::select(x, dplyr::all_of(vars), dplyr::all_of(by)) %>%
    # Remove labels (pivot does not work with labels)
    labelled::remove_var_label() %>%
    # Data is currently as variable per column, pivot to variable=value form.
    tidyr::pivot_longer(dplyr::all_of(vars), names_to = "variable") %>%
    # In variable=value form, group by the variable
    dplyr::group_by(.data$variable, !!by)
}




#' Title
#' @description Given
#' @param x
#' @param xorig
#' @param vars
#'
#' @return
#'
#' @examples
format_calculations_for_plotting <- function(x, xorig, vars) {
    x %>%
    # Explicitly ungroup the tibble, so there are no downstream issues
    dplyr::ungroup() %>%
    # Join the labels back in.
    dplyr::left_join(extract_var_labels(xorig),
                     by=c("variable"="variable")) %>%

    dplyr::left_join(
      tibble::enframe(vars,name="i", value="variable"),
                     by=c("variable" = "variable" )) %>%
    dplyr::arrange(.data$i) %>%
    dplyr::select(-.data$i) %>%
    dplyr::mutate(label = factor(.data$label, levels=unique(.data$label)))

}


#' Title
#'
#' @param x
#' @param vars
#' @param by
#'
#' @return
#' @export
#'
#' @examples
calculate_counts_yesno <- function(x, vars, by=NULL) {
  tidy_data_for_calculation(x, vars, by) %>%
    # Then compute statistics on the variable
    dplyr::summarize(
      count = cc_yesno(value),
      percent = count/n(),
      n = n()
    ) %>%
    format_calculations_for_plotting(x, vars)
}


#' Summarize variables that are paired
#'
#' @description Summarize a table `x` of variables `vars` paired by `by`.
#'
#' @details
#' This is a complicated function, but it provides a way to summarize multiple variable columns
#' by another column. An example will hopefully help.
#'
#' First, assume we a evaluation table `x`. It has a number of variables (numeric), such as
#' `a`, `b` and `c`. Further, this a pre/post condition which is stored in the variable `status`.
#'
#' We can look at the differences between `a`, `b` and `c` in pre/post status (in summarized form)
#' with the following:
#' ```
#' summarize_paired_info(x, vars=c(a,b,c), by = status ~ post - pre, subject_id=record_id)
#' ```
#'
#' This translates into summarizing vars (`a`,`b`,`c`) by taking the difference of the variable in question
#' (each of `a`,`b` and `c`) from the same subject (as indicated by `record_id`) between the `post` and `pre`
#' values of the `status` column. The result is a table of the following:
#'
#' ```
#' var = variable name (from input vars)
#' mean = mean paired summary (taken from by)
#' sd = standard deviation paired summary
#' n = count of pairs
#' p.value = wilcox signed rank (paired) test between levels (given in by)
#' Label = the labels (if any) of the variables.
#' ```
#'
#' @note This should be split into multiple different functions to make it easier to understand and to
#' allow for more reusability.
#' @param x
#' @param vars
#' @param by
#' @param subject_id
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
calculate_paired <- function(x, vars, by=status ~ post - pre, subject_id="record_id") {
  checkmate::assert_formula(by)
  checkmate::assert_vector(all.vars(by), len=3)

  subject_id <- rlang::ensym(subject_id)

  # For now, just extract the two variable names from the formula for computations below.
  by_values_1 <- all.vars(by)[2]
  by_values_2 <- all.vars(by)[3]

  x %>%
    # Remove the variable labels for this operation, since pivot doesn't like them.
    labelled::remove_var_label() %>%
    # Use only the selected variables, plus the grouping variables (record and status)
    dplyr::select({{subject_id}}, !!formula.tools::lhs(by), dplyr::all_of(vars)) %>%
    # Move all of the variables into rows (longer)
    tidyr::pivot_longer(cols=vars, names_to="variable",values_to="value") %>%
    # Then move pre/post into separate columns (wider)
    tidyr::pivot_wider(names_from = !!formula.tools::lhs(by), values_from=value) %>%
    # Now we can calculate differences between pre/post
    dplyr::mutate(differences = !!formula.tools::rhs(by)) %>%
    # Group by the variable (across subjects) %>%
    dplyr::group_by(variable) %>%
    # Calculate variable statistics
    dplyr::summarize(mean = mean(.data$differences),
              sd = sd(.data$differences),
              se = sd(.data$differences) / sqrt(dplyr::n()),
              n = dplyr::n(),
              p.value = wilcox.test(.data[[by_values_1]],.data[[by_values_2]],
                                    paired=TRUE, exact=ifelse(n>50,TRUE, FALSE))$p.value
    ) %>%
    format_calculations_for_plotting(x, vars)

}



