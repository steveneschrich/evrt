#' Choice data type
#'
#' @description A choice type is a "select all that apply" question. This translates into
#'  multiple responses for each participant.
#'
#' @details When counting these types of variables, one can
#'  include the number of participants that responded with a particular choice and show
#'  all possible choices.
#'
#' Possibilities include
#' - summarize_choice: Create a summarized table of the choices.
#' - plot_choice: Create a figure of the choices.
#' - gvsummary_choice: Create a flextable and plot of the choices.
#'
#' @name choice
NULL



#' Summarize choice variables to a table
#'
#' @param x A data frame
#' @param vars A list of variables (currently only one) to select. Note that as a
#'  choice variable, this corresponds to a choice set "stem", or the text preceding
#'  the `___N` in the name.
#' @param by An optional stratification of the choices when summarizing.
#' @param include_overall (TRUE) Include a column of overall counts. Only useful when
#'  stratifying (`by`).
#' @param output A string representing the desired output. Choices include:
#'  - "gtsummary" A [gtsummary::gtsummary] object.
#'  - "flextable" A [flextable::flextable] table with formatting applied to a gtsummary table.
#'  - "rmarkdown" Raw flextable output for use in rmarkdown (see [flextable::flextable_to_rmd()]
#'    for details). This is most suitable for running summarize in a loop or as list component.
#'
#' @return A summary table object which summarizes the counts of observations in
#' the choice variable(s). Results are in the format described in `output`.
#' @export
#'
summarize_choice <- function(x, vars = NULL, by = NULL, include_overall = TRUE,
                             output = c("gtsummary","flextable", "rmarkdown")) {
  stopifnot("Only one variable is supported for summarize_choice." = length(vars) == 1)
  output <- match.arg(output)

  # Extract the details of the set of variables corresponding the input
  choice_set <- get_choices(x, vars)

  # There is a problem when a choice has not selections (all 0's), gtsummary does
  # not want to show the 1 breakdown (which is 0%). We have to do a few things to
  # force the issue.
  x <- dplyr::mutate(
    x,
    dplyr::across(
      dplyr::all_of(choice_set$vars),
      \(.x) {
        factor(.x, levels=c("0","1")) |>
          labelled::set_variable_labels(.labels=labelled::var_label(.x))
      }
    )
  )

  tbl <- gtsummary::tbl_summary(
    x,
    include=dplyr::all_of(choice_set$vars),
    type = list(dplyr::everything() ~ "dichotomous"),
    by = dplyr::all_of(by),
    label =  purrr::map2(
      choice_set$vars,
      choice_set$choice,
      .f = \(x,y){
        as.formula(call("~",x,y))
      }
    ),
    value = list(dplyr::everything() ~ "1")
  ) |>
    gtsummary::modify_header(
      label = paste0("**", stringr::str_trim(choice_set$question), "**")
    ) |>
    gtsummary::modify_footnote(
      label = "Output may include multiple responses per participant."
    )
  # Include overall column (if by was set, otherwise it's meaningless).
  if ( !is.null(by) && include_overall )
    tbl <- gtsummary::add_overall(tbl, last = TRUE, col_label="**Total** N = {N}")

  # Decide on output: gtsummary, flextable or rmarkdown.
  if ( output == "flextable" || output == "rmarkdown") {
    tbl <- gtsummary::as_flex_table(tbl) |>
      evrt::style_flextable()
    if ( output == "rmarkdown" )
      tbl <- flextable::flextable_to_rmd(tbl)
  }

  tbl
}






#' Get a list of choices from a data frame
#'
#' @description A choice set is a collection of columns of a data frame
#'  that have a common question associated, but are indicators of the
#'  specific choice made (in a select all that apply). This function
#'  extracts the choices as a list of column names, choice values and
#'  the overall question asked.
#'
#' @param x A data frame with choices.
#' @param stem The variable stem that will be used to identify choices. Choice
#'  columns are named as `stem___1`, `stem___2`, etc.
#'
#' @return A list consisting of
#'  - vars: the column names of the choices fields.
#'  - question: the question asked (in common among choices).
#'  - choice: the specific choice corresponding to a `var`.
#'
#' @export
#'
get_choices <- function(x, stem) {
  var_list <- get_choice_list(x, stem)

  extract_choice_components(x, var_list)
}


#' Retrieve a choice list from a variable stem
#'
#' @description A list of choices are prepended with '___N' to indicate the
#'  choice. This function finds the choices from the variable stem from a
#'  data frame and returns the column names of the choices.
#'
#' @param x A data frame
#' @param stem A variable stem, for which choices are represented as 'stem___1'
#'  and 'stem___2', etc.
#'
#' @return A vector of column names in x that correspond to the choices identified
#'  for the stem.
#' @export
#'
#' @examples
#' get_choice_list(
#'  setNames(iris, paste("example",1:5,sep="___")),
#'  stem = "example"
#' )
get_choice_list <- function(x, stem) {
  choice_var_prefix <- paste0(stem, "___")

  var_index <- tidyr::starts_with(choice_var_prefix, vars = colnames(x))

  var_names <- colnames(x)[var_index]

  var_names
}

#' Extract choice components from a data frame
#'
#' @description A choice consists of a series of indicator columns for a
#'  variable. The label for each variable is the question asked followed
#'  by the specific choice (the text is `(choice=zzzz)`). This function
#'  extracts the variables names, choice text and question text into a list.
#'
#'
#' @param x A data frame with choice variables included.
#' @param vars A list of variables (assumed to be choices) from which to
#'  extract choice information.
#'
#' @return A list consisting of
#'  - vars: the column names of the choices fields.
#'  - question: the question asked (in common among choices).
#'  - choice: the specific choice corresponding to a `var`.
#' @export
#'
#'
extract_choice_components <- function(x, vars) {
  labs <- dplyr::select(x, dplyr::all_of(vars)) |>
    labelled::var_label(unlist = TRUE)

  matches <- stringr::str_match(labs, "^(.*)\\(choice=(.*)\\)")
  stopifnot(length(unique(matches[,2]))==1)

  list(
    vars = vars,
    question = unique(matches[,2]),
    choice = matches[,3]
  )
}



