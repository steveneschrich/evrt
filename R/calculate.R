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

  checkmate::assert_data_frame(x |> dplyr::select(dplyr::all_of(vars)), type=c("numeric"))

  stats <- x |>
    # Here we select all needed variables first.
    dplyr::select(dplyr::all_of(c(by, vars))) |>
    # Remove variable labels to avoid pivoting problems
    labelled::remove_labels() |>
    # Then make the data long for computing
    tidyr::pivot_longer(cols = dplyr::all_of(vars), names_to="variable") |>
    # Group by the "by" and variable for computations
    dplyr::group_by(dplyr::across(dplyr::all_of( c(by, "variable")))) |>
    # Perform computations group-wise
    dplyr::summarize(
      mean = mean(value, na.rm=TRUE),
      sd = sd(value, na.rm = TRUE),
      n = length(na.omit(value)),
      se = sd/n,
      .groups="drop"
    )


  # Annotate counts with variable labels (if they exist).
  stats <- extract_var_labels(x, vars) |>
    dplyr::left_join(stats, by="variable") |>
    dplyr::select(dplyr::all_of(by), variable, label, n, dplyr::everything())

  stats

}



#' Calculate counts at or above a threshold for variables
#'
#' @description Calculate the counts of entries exceeding a threshold value
#'  for a series of variables.
#'
#' @details
#'
#' @param x Data frame containing variables
#' @param vars List of variables to count number at or above threshold
#' @param threshold A string representing the threshold factor level.
#' @param by An optional grouping variable to arrange data by
#'
#' @return A data frame of counts (see details).
#' @export
#'
#' @examples
calculate_counts_at_or_above_threshold <- function(x, vars, threshold = 0, by=NULL) {

  # Verify that threshold is a valid level of all the variables.
  checkmate::assert_true(
      all(
        purrr::map_lgl( dplyr::select(x, dplyr::all_of(vars)),
                        ~threshold %in% levels(.))
      )
  )

  calculate_counts(x, vars, by, f = count_at_or_above_threshold, threshold = threshold)
}

#' Calculate counts for yes/no variables
#'
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

  calculate_counts(x, vars, by, f = count_yes)

}



#' Title
#'
#' @param x
#' @param vars
#' @param by
#' @param f
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
calculate_counts <- function(x, vars, by = NULL, f=count_length, ...) {
  counts <- x |>
    # Group by the "by" variable first, then all operations are relative to it.
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    # Summarize data (possibly grouped by "by").
    dplyr::summarize(
      # Calculate counts above threshold for each var
      dplyr::across(
        dplyr::all_of(vars),
        ~list(f(.x, ...))
      )
    ) |>
    # We have columns for each variable, transpose this to have many rows
    # corresponding to each column.
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to="variable",
      values_to = "count"
    ) |>
    tidyr::unnest("count") |>
    # Calculate percentages for each variable
    dplyr::mutate(percent = count/n)


  # Annotate counts with variable labels (if they exist).
  counts <- extract_var_labels(x, vars) |>
    dplyr::left_join(counts, by="variable") |>
    dplyr::select(dplyr::all_of(by), variable, label, count, n, percent)

  counts
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
calculate_counts_by_factor <- function(x, vars, by=NULL) {

  counts <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarize(dplyr::across(dplyr::all_of(vars), function(.x) {
      list(setNames(forcats::fct_count(.x), c("level","count")))
    })) |>
    tidyr::pivot_longer(cols=dplyr::all_of(vars), names_to="variable", values_to="level") |>
    tidyr::unnest(cols="level") |>
    dplyr::mutate(
      n = nrow(x),
      percent = count/n
    )

  # Annotate counts with variable labels (if they exist).
   counts <- extract_var_labels(x, vars) |>
    dplyr::left_join(counts, by="variable") |>
    dplyr::select(dplyr::all_of(by), variable, label, level, count, n, percent)



  counts

}



#' Tidy data for summarization (Deprecated)
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



